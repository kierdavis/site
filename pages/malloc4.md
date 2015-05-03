---
title: The malloc4 algorithm for memory allocation
---

`malloc4` is a small system for managing memory allocations, much like the
UNIX/Linux `malloc` system call.

The system divides up the heap space into chunks, maintaining a linked list of
empty chunks. A cleanup process is run periodically, which joins together
adjacent empty chunks.

## Glossary

`N`
  : A notation used in this document to refer to the number of bytes in a
    machine word.

heap
  : The region of memory that is controlled by this algorithm.

address
  : An `N`-byte integer that uniquely references a byte within the *heap*.

null
  : An `N`-byte value that is different from any value that represents a valid
    address in the heap. The same value must be used for all occurrences of
    *null*, in the context of this allocation system.

chunk
  : A section of the heap that consists of a *chunk header* followed by a
    *chunk body*.

chunk header
  : A single `N`-byte word that represents the size of the *chunk body*, in
    bytes.

chunk body
  : A sequence of bytes of length determined by the corresponding *chunk header*.
    Addresses returned by `malloc` and passed to `free` are pointers to chunk
    bodies.

allocated chunk
  : A *chunk* that has been returned by `malloc` and is now in control of user
    code.

free chunk
  : Either a *chunk* that has never been allocated, or a chunk that has been
    allocated and then returned to the allocation system by passing the chunk
    pointer to `free`.

free chunk list
  : A linked list of free *chunks*. A pointer to the first chunk in the list 
    (the head of the list) is stored in the *head pointer* variable. Each chunk
    contains a pointer to the next free chunk in the list in the first word of
    the *chunk body* (the second word of the whole chunk). Storing meaningful
    data into a region normally reserved for usage by user code does not cause a
    problem, since a chunk is never under the control of user code and in the
    free chunk list at the same time. The list is always kept sorted in order of
    ascending chunk address (i.e. each chunk only ever has a chunk with a
    higher address as its next-pointer).

head pointer
  : An `N`-byte variable/register/memory location holding a pointer to the first
    chunk in the free chunk list.

next-pointer
  : An `N`-byte pointer to the next chunk in the free chunk list. It is stored
    in first word of a chunk's body.

slice
  : The act of splitting a prospective chunk into two smaller chunks during
    allocation.

fit
  : The act of allocating an unsplittable chunk.

## Overview of the system

### Initialisation

When the system is started, a short initialisation procedure must occur. This
consists of three steps:

  * Setting the head pointer to point to the start of the heap.
  * Setting the first `N`-byte word in the heap to the size of heap minus `N`.
  * Setting the second `N`-byte word in the heap to `null`.

These steps create a single chunk in the heap, starting at the first available
address and extending to the end of the heap. The head pointer is set to point
to this chunk. The length of this chunk is the size of the heap minus `N`, since
it is the length of just the chunk body (which contains the word set in the
third step, but not the word set in the second step). The next-pointer for this
chunk is set to `null` to mark the end of the list.

#### Pseudocode implementation

    init() {
        head ← HEAPSTART
        [HEAPSTART] ← HEAPSIZE - N
        [HEAPSTART + N] ← NULL
    }

### Allocation

The first step in allocation is to round the requested allocation size up to the
next multiple of `N`. This ensures that all chunks start on `N`-byte boundaries,
which on some machines speeds up multibyte memory accesses.

The algorithm then iterates through the free chunk list. The size of each chunk
is fetched and compared against the requested allocation size.

  * If the chunk size is greater than the requested allocation size by at least
    a set number (`Z`) of words (i.e. if `chunksize >= reqsize + Z*N`), a
    *slice* occurs.
  * Otherwise, if the chunk size is greater than the requested allocation size
    (i.e. if `chunksize >= reqsize`), a *fit* occurs.
  * Otherwise, the chunk is considered too small and the next chunk in the free
    chunk list is examined.

If the end of the free chunk list is reached without either a *slice* or *fit*
event occurring, then there are no free chunks large enough and so the system
can be considered to be *out of memory*. The system may run a *coalesce*
(described in later sections) and then try the allocation algorithm again before
reporting an error condition.

A *slice* event means that the current chunk is fairly large in comparison to
the requested allocation size, and so the chunk will be split into two. One will
be used as the return value from `malloc` whilst the other will be returned to
the free chunk list.

The parameter `Z` (seen in the condition expression in the list above)
determines the minimum number of words that the size of the current chunk must
exceed the allocation size by. It can be assigned any value greater than one
(since a chunk must have a total size of at least two words). A typical value
for a processor with a small amount of memory is 9 (to leave room for a chunk
with an 8 word body).

To perform a slice:

  * The size of the chunk is decreased by `p` bytes, where `p` is equal to the
    requested allocation size plus `N`. This creates room for a new chunk with
    a body size equal to the requested allocation size.
  * The size field of this newly created chunk is set to the requested
    allocation size.
  * The address of the newly created chunk's body is returned to the caller.

A *fit* event means that the current chunk is just large enough for the
requested allocation size.

To perform a fit:

  * The previous chunk in the free chunk list has its next-pointer changed to
    point to the chunk after the one selected for the fit (i.e. it is set to be
    the same as the next-pointer of the current chunk). If there is no previous
    chunk, the head pointer is changed instead. If there is no next chunk (i.e.
    the next-pointer of the current chunk is `null`), the next-pointer of the
    previous chunk is set to `null` as well.
  * The address of the current chunk's body is returned to the caller.

#### Pseudocode implementation

    malloc(size) {
        -- Round size up to word boundary
        size = (size + N - 1) & -N
        
        last_chunk ← NULL
        this_chunk ← head
        
        while this_chunk != NULL {
            this_chunk_size ← [this_chunk]
            next_chunk ← [this_chunk + N]
            
            if this_chunk_size >= size + Z*N {
                -- slice
                this_chunk_size ← this_chunk_size - size - N
                [this_chunk] ← this_chunk_size
                new_chunk ← this_chunk + N + this_chunk_size
                [new_chunk] ← size
                return new_chunk + N
            }
            
            else if this_chunk_size >= size {
                -- fit
                if last_chunk == NULL {
                    head ← next_chunk
                }
                else {
                    [last_chunk + N] ← next_chunk
                }
                return this_chunk + N
            }
            
            else {
                last_chunk ← this_chunk
                this_chunk ← next_chunk
            }
        }
        
        -- Out of memory
        return NULL
    }

### Deallocation

Allocated chunks are deallocated by adding them to the free chunk list. Since
the free chunk list is to be kept sorted, the system iterates through the list
until it finds a suitable place to insert the chunk, then insert it. This
mechanism also allows the condition of an already unallocated chunk being
passed to `free` to be detected and handled.

#### Pseudocode implementation

    free(ptr) {
        insert_chunk ← ptr - N
        
        last_chunk ← NULL
        this_chunk ← head
        
        loop {
            if insert_chunk == this_chunk {
                -- Already unallocated, or NULL.
                return
            }
            
            else if this_chunk == NULL || insert_chunk < this_chunk {
                if last_chunk == NULL {
                    -- Insert insert_chunk at start of list.
                    head ← insert_chunk
                }
                else {
                    -- Insert insert_chunk between last_chunk and this_chunk.
                    [last_chunk + N] ← insert_chunk
                }
                [insert_chunk + N] ← this_chunk
                return
            }
            
            else {
                last_chunk ← this_chunk
                this_chunk ← [this_chunk + N]
            }
        }
    }

### Cleanup (coalescing)

Coalescing is the process of joining together adjacent free chunks, in order to
increase the mean size of free chunks. The system iterates over the free chunk
list, merging any two directly adjacent chunks into one larger one.

#### Pseudocode implementation

    coalesce() {
        this_chunk ← head
        
        while this_chunk != NULL {
            this_chunk_size ← [this_chunk]
            next_chunk ← [this_chunk + N]
            
            if this_chunk + N + this_chunk_size == next_chunk {
                -- this_chunk and next_chunk are directly adjacent
                next_chunk_size ← [next_chunk]
                -- Expand this_chunk to also contain next_chunk's header & body.
                [this_chunk] ← this_chunk_size + N + next_chunk_size
                -- Copy next-pointer from next chunk to this chunk.
                [this_chunk + N] ← [next_chunk + N]
                -- Do not move this_chunk forward since we may be able to
                -- coalesce again.
            }
            
            else {
                this_chunk ← next_chunk
            }
        }
    }
