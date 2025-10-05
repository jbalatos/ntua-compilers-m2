#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#ifndef ADDR_OF
	#define ADDR_OF(x) ((typeof(x)[1]){(x)})
#endif

#pragma region DYNAMIC ARRAY
#define arr_empty(a)          ((a) == NULL || arr_header(a)->length == 0)
#define arr_len(a) (ptrdiff_t)((a) ? arr_header(a)->length   : 0ul)
#define arr_cap(a) (ptrdiff_t)((a) ? arr_header(a)->capacity : 0ul)
#define arr_ulen(a)           ((a) ? arr_header(a)->length   : 0ul)
#define arr_ucap(a)           ((a) ? arr_header(a)->capacity : 0ul)

#define arr_init(a)        (void)((a) = NULL)
#define arr_setcap(a, cap) arr_maybegrow(a, 0, cap)
#define arr_free(a) ((void)((a)                        \
			? (free(arr_header(a))) : (0) ))

#define arr_assign(a, cap, v)  do {                                \
	arr_regrow(a, 0, cap);                                     \
	for (size_t __tmp__ = 0; __tmp__ < arr_ulen(a); ++__tmp__) \
		a[__tmp__] = (v);                                  \
} while (0)

#define arr_push(a, v) (                          \
		arr_maybegrow(a, 1, 0),           \
		(a)[arr_header(a)->length++] = (v))
#define arr_pop(a)  ((void)(--arr_header(a)->length))
#define arr_back(a)  (a)[arr_header(a)->length - 1]
#define arr_ins(a, pos, v) (                               \
		arr_maybegrow(a, 1, 0),                    \
		memmove(a + pos + 1, a + pos,              \
			(arr_ulen(a) - pos) * sizeof(*a)), \
		arr_header(a)->length += 1,                \
		a[pos] = (v)                               )
#define arr_del(a, pos) (                                  \
		memmove(a + pos + 1, a + pos,              \
			(arr_ulen(a) - pos) * sizeof(*a)), \
		arr_header(a)->length -= 1                 )
#define arr_swapdel(a, pos) (           \
		a[pos] = arr_back(a),   \
		--arr_header(a)->length )
#pragma endregion

#pragma region HASH SET
#define hs_empty arr_empty
#define hs_len   arr_len
#define hs_ulen  arr_ulen

#define hs_get(h, k)  (hm_geti(h, k) == -1)
#define hs_getp(h, k) (hm_geti(h, k) == -1 ? NULL : (h) + hm_tmp(h))
#define hs_geti        hm_geti

#define hs_put(h, k)   hm_puts(h, hm_hash(k), (k))
#define hs_del         hm_del
#define hs_free        hm_free
#pragma endregion

#pragma region HASH MAP
#define hm_empty(h) ((h) == NULL)
#define hm_len      arr_len
#define hm_ulen     arr_ulen

#define hm_geti(h, k)  hm_get_int((void*)(h), hm_hash(k))
#define hm_gets(h, k) (hm_geti(h, k) == -1                  \
		? (typeof(   *h   )){} : (h)[hm_tmp(h)])
#define hm_get(h, k)  (hm_geti(h, k) == -1                       \
		? (typeof(h->value)){0} : (h)[hm_tmp(h)].value)
#define hm_getp(h, k) (hm_geti(h, k) == -1 ? NULL : (h) + hm_tmp(h))

#define hm_put(h, k, v) hm_puts(                           \
		h, hm_hash(k),                             \
		((typeof(*h)){ .key = (k), .value = (v) }) \
		)
#define hm_puts(h, hash, s...) (                            \
		hm_maybegrow(h, 1, 0),                      \
		hm_put_int((void*)h, hash, arr_ulen(h))     \
		< arr_len(h) ? 0 : ++arr_header(h)->length, \
		(h)[hm_tmp(h)] = (s)                        )

#define hm_del(h, k) (hm_del_int((void*)(h), hm_hash(k),            \
			hm_hash(arr_back(h).key), sizeof(*h)) == -1 \
			? 0 : arr_swapdel(h, hm_tmp(h))             )
#define hm_free(h) ((void)((h)                                                  \
			? (free(arr_header(h)->hash_table),free(arr_header(h))) \
			: (0)                                                  ))
#pragma endregion

#pragma region DECLARATIONS
typedef struct {
	size_t hash;
	size_t index;
	size_t dist;
} hm_cell_t;

typedef struct {
	size_t length, capacity;
	hm_cell_t *hash_table;
	ptrdiff_t tmp;
} arr_header_t;

#define arr_header(ptr) ((arr_header_t*)ptr - 1)
#define hm_tmp(h)        (arr_header(h)->tmp)

extern void*     arr_grow_int(void*, size_t, size_t, size_t);
extern size_t    hm_hash_bytes(const char*, size_t);
extern ptrdiff_t hm_get_int(void*, size_t);
extern ptrdiff_t hm_put_int(void*, size_t, size_t);
extern ptrdiff_t hm_del_int(void*, size_t, size_t, size_t);
extern void      hm_migrate(void*, void*, size_t);
extern void*     hm_grow_int(void*, size_t, size_t, size_t);
#pragma endregion

#ifdef DA_IMPLEMENT
#pragma region IMPLEMENTATION
/** dynamic array */
#define arr_maybegrow(a, to_add, min_cap)                            \
	(arr_ulen(a) + to_add > arr_ucap(a) || min_cap > arr_ucap(a) \
	 ? arr_grow(a, to_add, min_cap) : 0                          )
#define arr_grow(a, to_add, min_cap) (void)(                                \
		(a) = arr_grow_int((void*)(a), to_add, min_cap, sizeof(*a)) \
	)

void * __attribute__((unused))
arr_grow_int (void *a, size_t to_add, size_t min_cap,
		size_t elemsize)
{
	size_t new_cap = arr_ulen(a) + to_add;
	void *ret;

	if (new_cap < min_cap) new_cap = min_cap;
	if (new_cap <= arr_ucap(a)) return a;
	if (new_cap < 2 * arr_ucap(a)) new_cap = 2 * arr_ucap(a);
	ret = sizeof(arr_header_t) + 
		malloc(new_cap * elemsize + sizeof(arr_header_t));
	*arr_header(ret) = (arr_header_t){ .capacity = new_cap };
	if (a) {
		arr_header(ret)->length = arr_ulen(a);
		memcpy(ret, a, arr_ulen(a) * elemsize);
		free(arr_header(a));
	}

	return ret;
}

/** hash_map */
static size_t hm_seed = 0x31415926;

#define hm_maybegrow(a, to_add, min_cap)                             \
	(arr_ulen(a) + to_add > arr_ucap(a) || min_cap > arr_ucap(a) \
	 ? hm_grow(a, to_add, min_cap) : 0                           )
#define hm_grow(a, to_add, min_cap) (void)(                              \
		(a) = hm_grow_int((void*)(a), to_add +                   \
			(size_t)(arr_ulen(a) == 0), min_cap, sizeof(*a)) )

#define hm_hash(key) hm_hash_bytes(_Generic((key),           \
			      char *: (key),                 \
			const char *: (key),                 \
			default     : (char *)(ADDR_OF(key)) \
		), _Generic((key),                           \
			      char *: 0,                     \
			const char *: 0,                     \
			default     : sizeof(key))           )

#define HM_PRINT(h, log) do { \
	log("Map print (%lu/%lu):\n", arr_header(h)->length, arr_header(h)->capacity); \
	for (size_t __tmp__ = 0; __tmp__ < arr_ucap(h); ++__tmp__) { \
		log("\t{%ld %ld %ld}", \
				arr_header(h)->hash_table[__tmp__].hash, \
				arr_header(h)->hash_table[__tmp__].index, \
				arr_header(h)->hash_table[__tmp__].dist); \
		if (arr_header(h)->hash_table[__tmp__].hash) { \
			log(" ==> (%u: %s", \
				(h)[arr_header(h)->hash_table[__tmp__].index].key \
				(h)[arr_header(h)->hash_table[__tmp__].index].value); \
			log(")"); \
		} \
		log("\n"); \
	} \
	log("-----\n"); \
} while (0)

size_t __attribute__((unused))
hm_hash_bytes (const char *ptr, size_t len) // OAAT
{
	size_t hash = hm_seed;
	bool str = len == 0;

	for (; str ? *ptr != '\0' : len; len--, ptr++ ) {
		hash += *ptr;
		hash += hash << 10;
		hash ^= hash >> 6;
	}
	hash += hash << 3;
	hash ^= hash >> 11;
	hash += hash << 15;
	if (hash == 0)
		hash ^= 0xAAAAAAAAAAAAAAAA;
	return hash;
}

ptrdiff_t __attribute__((unused))
hm_get_int (void *map, size_t hash)
{
	if (!map) return -1;

	hm_cell_t *table = arr_header(map)->hash_table;
	size_t i = hash % arr_ucap(map), dist = 0;

	for (; dist < arr_ucap(map); i = (i + 1) % arr_ucap(map), ++dist) {
		if (table[i].hash == hash)
			return hm_tmp(map) = table[i].index;
		if (table[i].hash == 0 || table[i].dist < dist)
			return hm_tmp(map) = -1;
	}
	return hm_tmp(map) = -1;
}

ptrdiff_t __attribute__((unused))
hm_put_int (void *map, size_t hash, size_t index)
{
	hm_cell_t *table = arr_header(map)->hash_table;
	size_t i = hash % arr_ucap(map);
	hm_cell_t cell = { .hash = hash, .index = index, .dist = 0 },
		  tmp;

	for (; true; i = (i + 1) % arr_ucap(map), ++(cell.dist)) {
		if (table[i].hash == cell.hash)
			return hm_tmp(map) = table[i].index;
		if (table[i].hash == 0) {
			table[i] = cell;
			break;
		}
		if (cell.dist > table[i].dist) {
			tmp = table[i];
			table[i] = cell;
			cell = tmp;
		}
	}

	// eprintf("Map contains:\n");
	// for (size_t j=0; j<arr_ucap(map); ++j)
		// log("\t{%lu %lu %lu}\n", table[j].hash, table[j].index, table[j].dist);

	return hm_tmp(map) = index;
}

ptrdiff_t __attribute__((unused))
hm_del_int (void *map, size_t hash, size_t last, Unused size_t elemsize)
{
	hm_cell_t *table = arr_header(map)->hash_table;
	size_t i = hash % arr_ucap(map), dist = 0;
	hm_tmp(map) = -1;

	for (; dist < arr_ucap(map); i = (i + 1) % arr_ucap(map), ++dist) {
		if (table[i].hash == hash) {
			hm_tmp(map) = table[i].index;
			break;
		}
		if (table[i].hash == 0 || table[i].dist > dist)
			break;
	}
	if (hm_tmp(map) == -1)
		return hm_tmp(map);

	table[i] = (hm_cell_t){};
	/* shift cells forward */
	for (++i, dist = 0; !table[i].hash && !table[i].dist
			&& dist < arr_ucap(map); i = (i + 1) % arr_ucap(map)) {
		table[(i - 1 + arr_ucap(map)) % arr_ucap(map)] = table[i];
		table[i] = (hm_cell_t){};
	}
	/* update cell pointing to last element */
	for (i = last % arr_ucap(map); true; i = (i + 1) % arr_ucap(map)) {
		if (table[i].hash == last) {
			table[i].index = hm_tmp(map);
			break;
		}
	}
	return hm_tmp(map);
}

void __attribute__((unused))
hm_migrate (void *dest, void *src, size_t elemsize)
{
	size_t i;
	hm_cell_t *table = arr_header(src)->hash_table;

	memcpy(dest, src, arr_ulen(src) * elemsize);
	arr_header(dest)->length = arr_ulen(src);
	for (i=0; i<arr_ucap(src); ++i) if (table[i].hash) {
		hm_put_int(dest, table[i].hash, table[i].index);
	}
}

void * __attribute__((unused))
hm_grow_int (void *a, size_t to_add, size_t min_cap, size_t elemsize)
{
	size_t new_cap = arr_ulen(a) + to_add;
	void *ret;

	if (new_cap < min_cap) new_cap = min_cap;
	if (new_cap <= arr_ucap(a)) return a;
	if (min_cap < 2 * arr_ucap(a)) new_cap = 2 * arr_ucap(a);
	ret = sizeof(arr_header_t) + 
		malloc(new_cap * elemsize + sizeof(arr_header_t));
	*arr_header(ret) = (arr_header_t){
		.capacity = new_cap,
		.hash_table = malloc(new_cap * sizeof(hm_cell_t))
	};
	memset(arr_header(ret)->hash_table, 0, new_cap * sizeof(hm_cell_t));
	if (a) {
		hm_migrate(ret, a, elemsize);
		free(arr_header(a)->hash_table);
		free(arr_header(a));
	}

	return ret;
}
#pragma endregion
#endif //DA_IMPLEMENT
