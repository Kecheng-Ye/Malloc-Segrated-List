/*
 ******************************************************************************
 *                                   mm.c                                     *
 *           64-bit struct-based implicit free list memory allocator          *
 *                  15-213: Introduction to Computer Systems                  *
 *                                                                            *
 *  ************************************************************************  *
 *             Modified version of one provided to students in malloc lab     *
 *  This version supports malloc and free over a fixed-size heap              *
 *                                                                            *
 *  ************************************************************************  *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stddef.h>
#include <inttypes.h>
#include <string.h>

#include "mm.h"
#include "memlib.h"

team_t team = {
		/* Team name */
		"ateam",
		/* First member's full name */
		"Harry Bovik",
		/* First member's email address */
		"bovik@cs.cmu.edu",
		/* Second member's full name (leave blank if none) */
		"",
		/* Second member's email address (leave blank if none) */
		""
};

/* Basic constants */

typedef uint64_t word_t;

// Word and header size (bytes)
static const size_t wsize = sizeof(word_t);

// Double word size (bytes)
static const size_t dsize = 2 * sizeof(word_t);

/*
  Minimum useable block size (bytes):
  two words for header & footer, two words for payload
*/
static const size_t min_block_size = 4 * sizeof(word_t);

// Mask to extract allocated bit from header
static const word_t alloc_mask = 0x1;

static const word_t prev_alloc_mask = 0b10;

/*
 * Assume: All block sizes are a multiple of 16
 * and so can use lower 4 bits for flags
 */
static const word_t size_mask = ~(word_t) 0xF;

static const size_t chunksize = (1 << 12);    // requires (chunksize % 16 == 0)

/*
  All blocks have both headers and footers

  Both the header and the footer consist of a single word containing the
  size and the allocation flag, where size is the total size of the block,
  including header, (possibly payload), unused space, and footer
*/

/* Representation of the header and payload of one block in the heap */
typedef struct block
{
	word_t header;
	/*
	 * We don't know what the size of the payload will be, so we will
	 * declare it as a zero-length array.  This allow us to obtain a
	 * pointer to the start of the payload.
	 */
	unsigned char payload[0];

} block_t;


/* Global variables */

// Pointer to first block
static block_t *heap_start = NULL;
// Pointer to last block.  This is an empty, but allocated block
static block_t *heap_end = NULL;
static block_t *next_fit_ptr = NULL;

/* Function prototypes for internal helper routines */

static block_t * (*find_fit)(size_t asize);
static block_t *first_fit(size_t asize);
static block_t *next_fit(size_t asize);
static block_t *best_fit(size_t asize);
static void coalesce_block(block_t *block);
static void split_block(block_t *block, size_t asize);

static size_t round_up(size_t size, size_t n);
static word_t pack(size_t size, bool alloc, bool prev_alloc);

static size_t extract_size(word_t header);
static size_t get_size(block_t *block);

static bool extract_alloc(word_t header);
static bool get_alloc(block_t *block);
static bool Is_prev_alloc(block_t *block);
static bool extract_prev_alloc(word_t word);

static void write_header(block_t *block, size_t size, bool alloc, bool prev_alloc);
static void write_footer(block_t *block, size_t size, bool alloc, bool prev_alloc);

static block_t *payload_to_header(void *bp);
static void *header_to_payload(block_t *block);
static word_t *header_to_footer(block_t *block);

static block_t *find_next(block_t *block);
static word_t *find_prev_footer(block_t *block);
static block_t *find_prev(block_t *block);
static block_t *extend_heap(size_t size);
void mm_status(FILE *fp);
static word_t get_payload_size(block_t *block);

void log_block(block_t *block){
	bool is_allocated = get_alloc(block);
	printf("Block address %p,  size = %zd, allocated = %s, prev_allocated = %s",
	       block,
	       get_size(block),
	       is_allocated ? "Y" : "N",
	       Is_prev_alloc(block) ? "Y" : "N");

	printf("\n");
}

static const int fit_type = 1; // 0 for first fit, 1 for next fit, 2 for best fit

/*
  Initialize a heap to have byte_count bytes starting at address start.
  Assume start and byte_count are multiples of dsize.
 */
int mm_init()
{
	word_t *start = (word_t*)(mem_sbrk(2 * wsize));
	if(start == (void *)-1) {
		return -1;
	}

	start[0] = pack(0, true, true); // Prologue footer
	start[1] = pack(0, true, true); // Epilogue header

	// Extend the empty heap with a free block of chunksize bytes
	if ((heap_start = extend_heap(chunksize)) == NULL)
	{
		return -1;
	}

	if(fit_type == 0) {
		find_fit = first_fit;
	}else if(fit_type == 1) {
		find_fit = next_fit;
		next_fit_ptr = NULL;
	}else if(fit_type == 2){
		find_fit = best_fit;
	}

	return 0;
}

/*
 * Allocate space for payload of size bytes
 */
void *mm_malloc(size_t size)
{
	size_t asize;      // Allocated block size
	block_t *block = NULL;
	void *bp = NULL;

	if (size == 0) // Ignore spurious request
		return bp;

	// !!!!!
	// only one header but no footer for allocated block
	asize = round_up(size + wsize, dsize);

	while((block = find_fit(asize)) == NULL) {
		extend_heap(chunksize);
	}

	// Mark block as allocated
	size_t block_size = get_size(block);
	write_header(block, block_size, true, Is_prev_alloc(block));
	// !!!!!
	//	 no need to write footer
	//	 write_footer(block, block_size, true);

	// Try to split the block if too large
	split_block(block, asize);
	bp = header_to_payload(block);

	return bp;
}


/* Free allocated block */
void mm_free(void *bp)
{
	if (bp == NULL)
		return;

	block_t *block = payload_to_header(bp);
	size_t size = get_size(block);

	// The block should be marked as allocated
	if (!get_alloc(block)) {
		fprintf(stderr, "ERROR.  Attempted to free unallocated block\n");
		exit(1);
	}

	// Mark the block as free
	write_header(block, size, false, Is_prev_alloc(block));
	write_footer(block, size, false, Is_prev_alloc(block));

	// Try to coalesce the block with its neighbors
	coalesce_block(block);
}

void *mm_realloc(void *ptr, size_t size) {
	block_t *block = payload_to_header(ptr);
	size_t copysize;
	void *newptr;

	// If size == 0, then free block and return NULL
	if (size == 0)
	{
		free(ptr);
		return NULL;
	}

	// If ptr is NULL, then equivalent to malloc
	if (ptr == NULL)
	{
		return malloc(size);
	}

	// Otherwise, proceed with reallocation
	newptr = malloc(size);
	// If malloc fails, the original block is left untouched
	if (!newptr)
	{
		return NULL;
	}

	// Copy the old data
	copysize = get_payload_size(block); // gets size of old payload
	if(size < copysize)
	{
		copysize = size;
	}
	memcpy(newptr, ptr, copysize);

	// Free the old block
	free(ptr);

	return newptr;
}

/* Print status of every block in heap */
void mm_status(FILE *fp) {
	block_t *block = heap_start;
	printf("*******************************\n");
	while (block != heap_end) {
		fprintf(fp, " Block address %p,  size = %zd, allocated = %s, prev_allocated = %s\n",
		        block,
		        get_size(block),
		        get_alloc(block) ? "Y" : "N",
		        Is_prev_alloc(block) ? "Y" : "N");
		block = find_next(block);
	}
	fprintf(fp, "Heap end address %p,  size = %zd, allocated = %s, prev_allocated = %s\n",
	        heap_end,
	        get_size(heap_end),
	        get_alloc(heap_end) ? "Y" : "N",
	        Is_prev_alloc(heap_end) ? "Y" : "N");
	printf("*******************************\n");


}

/******** The remaining content below are helper and debug routines ********/


/*
 * Attempt to coalesce block with its predecessor and successor
 */
static void coalesce_block(block_t *block)
{

	size_t size = get_size(block);

	block_t *block_next = find_next(block);
	block_t *block_prev = find_prev(block);

	bool prev_alloc = Is_prev_alloc(block);
	bool next_alloc = get_alloc(block_next);

	if (prev_alloc && next_alloc)              // Case 1
	{
		// Nothing to do
	}

	else if (prev_alloc && !next_alloc)        // Case 2
	{
		size += get_size(block_next);
		write_header(block, size, false, prev_alloc);
		write_footer(block, size, false, prev_alloc);
	}

	else if (!prev_alloc && next_alloc)        // Case 3
	{
		size += get_size(block_prev);
		bool Is_prev_prev_alloc = Is_prev_alloc(block_prev);
		write_header(block_prev, size, false, Is_prev_prev_alloc);
		write_footer(block_prev, size, false, Is_prev_prev_alloc);
		block = block_prev;
	}

	else                                        // Case 4
	{
		size += get_size(block_next) + get_size(block_prev);
		write_header(block_prev, size, false, Is_prev_alloc(block_prev));
		write_footer(block_prev, size, false, Is_prev_alloc(block_prev));
		block = block_prev;
	}
}


/*
 * See if new block can be split one to satisfy allocation
 * and one to keep free
 */
static void split_block(block_t *block, size_t asize)
{
	size_t block_size = get_size(block);

	if ((block_size - asize) >= min_block_size)
	{
		block_t *block_next;
		write_header(block, asize, true, Is_prev_alloc(block));

		block_next = find_next(block);
		write_header(block_next, block_size - asize, false, true);
		write_footer(block_next, block_size - asize, false, true);
	}
}


/*
 * Find a free block that of size at least asize
 * using first-fit discipline
 */
static block_t *first_fit(size_t asize) {
	block_t *block;

	for (block = heap_start; block != heap_end;
	     block = find_next(block))
	{
		if (!(get_alloc(block)) && (asize <= get_size(block)))
			return block;
	}
	return NULL; // no fit found
}

static block_t *next_fit(size_t asize) {
	if(next_fit_ptr == NULL) next_fit_ptr = heap_end;
	block_t *end = next_fit_ptr;

	do{
		if(next_fit_ptr == heap_end) {
			next_fit_ptr = heap_start;
		}else{
			next_fit_ptr = find_next(next_fit_ptr);
		}


		if (!(get_alloc(next_fit_ptr)) && (asize <= get_size(next_fit_ptr))) {
			block_t* result = next_fit_ptr;
			return result;
		}
	}while(next_fit_ptr != end);

	return NULL; // no fit found
}

static block_t *best_fit(size_t asize) {
	block_t *block;
	block_t *best_block = NULL;

	for (block = heap_start; block != heap_end;
	     block = find_next(block))
	{
		if (!(get_alloc(block)) && (asize <= get_size(block))) {
			if(best_block == NULL || (get_size(best_block) > get_size(block))){
				best_block = block;
			}
		}
	}
	return best_block; // no fit found
}

static block_t *extend_heap(size_t size)
{
	void *bp;

	// Allocate an even number of words to maintain alignment
	size = round_up(size, dsize);
	if ((bp = mem_sbrk(size)) == (void *)-1)
	{
		return NULL;
	}

	// Initialize free block header/footer
	block_t *block = payload_to_header(bp);
	bool is_prev_allocate = extract_alloc(*find_prev_footer(block));
	write_header(block, size, false, is_prev_allocate);
	write_footer(block, size, false, is_prev_allocate);
	// Create new epilogue header
	block_t *block_next = find_next(block);
	write_header(block_next, 0, true, false);
	heap_end = block_next;

	// Coalesce in case the previous block was free
	coalesce_block(block);
	return block;
}

/*
 *****************************************************************************
 * The functions below are short wrapper functions to perform                *
 * bit manipulation, pointer arithmetic, and other helper operations.        *
 *****************************************************************************
 */


/*
 * round_up: Rounds size up to next multiple of n
 */
static size_t round_up(size_t size, size_t n)
{
	return n * ((size + (n-1)) / n);
}


/*
 * pack: returns a header reflecting a specified size and its alloc status.
 *       If the block is allocated, the lowest bit is set to 1, and 0 otherwise.
 */
static word_t pack(size_t size, bool alloc, bool prev_alloc)
{
	word_t result = size;

	if(alloc) {
		result |= alloc_mask;
	}

	if(prev_alloc) {
		result |= prev_alloc_mask;
	}

	return result;
}

/*
 * extract_size: returns the size of a given header value based on the header
 *               specification above.
 */
static size_t extract_size(word_t word)
{
	return (word & size_mask);
}


/*
 * get_size: returns the size of a given block by clearing the lowest 4 bits
 *           (as the heap is 16-byte aligned).
 */
static size_t get_size(block_t *block)
{
	return extract_size(block->header);
}

/*
 * extract_alloc: returns the allocation status of a given header value based
 *                on the header specification above.
 */
static bool extract_alloc(word_t word)
{
	return (bool) (word & alloc_mask);
}

/*
 * get_alloc: returns true when the block is allocated based on the
 *            block header's lowest bit, and false otherwise.
 */
static bool get_alloc(block_t *block)
{
	return extract_alloc(block->header);
}

static bool extract_prev_alloc(word_t word)
{
	return (bool) (word & prev_alloc_mask);
}


static bool Is_prev_alloc(block_t *block)
{
	return extract_prev_alloc(block->header);
}



/*
 * write_header: given a block and its size and allocation status,
 *               writes an appropriate value to the block header.
 */
static void write_header(block_t *block, size_t size, bool alloc, bool prev_alloc)
{
	block->header = pack(size, alloc, prev_alloc);
}


/*
 * write_footer: given a block and its size and allocation status,
 *               writes an appropriate value to the block footer by first
 *               computing the position of the footer.
 */
static void write_footer(block_t *block, size_t size, bool alloc, bool prev_alloc)
{
	if(get_alloc(block)) {
		printf("!!!!! Wrong, try to write foot on a allocated block !!!!!\n");
		log_block(block);
	}

	word_t *footerp = header_to_footer(block);
	*footerp = pack(size, alloc, prev_alloc);
}


/*
 * find_next: returns the next consecutive block on the heap by adding the
 *            size of the block.
 */
static block_t *find_next(block_t *block)
{
	return (block_t *) ((unsigned char *) block + get_size(block));
}


/*
 * find_prev_footer: returns the footer of the previous block.
 */
static word_t *find_prev_footer(block_t *block)
{
	// Compute previous footer position as one word before the header
	return &(block->header) - 1;
}


/*
 * find_prev: returns the previous block position by checking the previous
 *            block's footer and calculating the start of the previous block
 *            based on its size.
 */
static block_t *find_prev(block_t *block)
{
	word_t *footerp = find_prev_footer(block);
	size_t size = extract_size(*footerp);
	return (block_t *) ((unsigned char *) block - size);
}


/*
 * payload_to_header: given a payload pointer, returns a pointer to the
 *                    corresponding block.
 */
static block_t *payload_to_header(void *bp)
{
	return (block_t *) ((unsigned char *) bp - offsetof(block_t, payload));
}


/*
 * header_to_payload: given a block pointer, returns a pointer to the
 *                    corresponding payload.
 */
static void *header_to_payload(block_t *block)
{
	return (void *) (block->payload);
}


/*
 * header_to_footer: given a block pointer, returns a pointer to the
 *                   corresponding footer.
 */
static word_t *header_to_footer(block_t *block)
{
	return (word_t *) (block->payload + get_size(block) - dsize);
}

static word_t get_payload_size(block_t *block)
{
	size_t asize = get_size(block);
	return asize - dsize;
}












