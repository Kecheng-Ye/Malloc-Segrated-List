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

/* Function prototypes for internal helper routines */

static block_t *find_fit(size_t asize);
static void coalesce_block(block_t *block);
static void split_block(block_t *block, size_t asize);

static size_t round_up(size_t size, size_t n);
static word_t pack(size_t size, bool alloc);

static size_t extract_size(word_t header);
static size_t get_size(block_t *block);

static bool extract_alloc(word_t header);
static bool get_alloc(block_t *block);

static void write_header(block_t *block, size_t size, bool alloc);
static void write_footer(block_t *block, size_t size, bool alloc);

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
	printf("  Block address %p,  size = %zd, allocated = %s\n",
	       block, get_size(block), get_alloc(block) ? "Y" : "N");
}

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

	start[0] = pack(0, true); // Prologue footer
	start[1] = pack(0, true); // Epilogue header


	// Extend the empty heap with a free block of chunksize bytes
	if ((heap_start = extend_heap(chunksize)) == NULL)
	{
		return -1;
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

	// Adjust block size to include overhead and to meet alignment requirements
	asize = round_up(size + dsize, dsize);



	while((block = find_fit(asize)) == NULL) {
		extend_heap(chunksize);
	}

	// Mark block as allocated
	size_t block_size = get_size(block);
	write_header(block, block_size, true);
	write_footer(block, block_size, true);

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
	write_header(block, size, false);
	write_footer(block, size, false);

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
	while (block != heap_end) {
		fprintf(fp, "  Block address %p,  size = %zd, allocated = %s\n",
		        block, get_size(block), get_alloc(block) ? "Y" : "N");
		block = find_next(block);
	}

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

	bool prev_alloc = extract_alloc(*find_prev_footer(block));
	bool next_alloc = get_alloc(block_next);

	if (prev_alloc && next_alloc)              // Case 1
	{
		// Nothing to do
	}

	else if (prev_alloc && !next_alloc)        // Case 2
	{
		size += get_size(block_next);
		write_header(block, size, false);
		write_footer(block, size, false);
	}

	else if (!prev_alloc && next_alloc)        // Case 3
	{
		size += get_size(block_prev);
		write_header(block_prev, size, false);
		write_footer(block_prev, size, false);
		block = block_prev;
	}

	else                                        // Case 4
	{
		size += get_size(block_next) + get_size(block_prev);
		write_header(block_prev, size, false);
		write_footer(block_prev, size, false);
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
		write_header(block, asize, true);
		write_footer(block, asize, true);

		block_next = find_next(block);
		write_header(block_next, block_size - asize, false);
		write_footer(block_next, block_size - asize, false);
	}
}


/*
 * Find a free block that of size at least asize
 * using first-fit discipline
 */
static block_t *find_fit(size_t asize)
{
	block_t *block;

	for (block = heap_start; block != heap_end;
	     block = find_next(block))
	{
		if (!(get_alloc(block)) && (asize <= get_size(block)))
			return block;
	}
	return NULL; // no fit found
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
	write_header(block, size, false);
	write_footer(block, size, false);
	// Create new epilogue header
	block_t *block_next = find_next(block);
	write_header(block_next, 0, true);
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
static word_t pack(size_t size, bool alloc)
{
	return alloc ? (size | alloc_mask) : size;
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


/*
 * write_header: given a block and its size and allocation status,
 *               writes an appropriate value to the block header.
 */
static void write_header(block_t *block, size_t size, bool alloc)
{
	block->header = pack(size, alloc);
}


/*
 * write_footer: given a block and its size and allocation status,
 *               writes an appropriate value to the block footer by first
 *               computing the position of the footer.
 */
static void write_footer(block_t *block, size_t size, bool alloc)
{
	word_t *footerp = header_to_footer(block);
	*footerp = pack(size, alloc);
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












