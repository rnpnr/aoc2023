/* AOC 2023: Day 3 - Gear Ratios
 *
 * Suppose you have the following visual representation
 * of some schematic:
 *
 * 467..114.3
 * 35*.......
 * ......633.
 * ......#...
 * 617*......
 * .....+.58.
 * ..592.....
 * ......755.
 * .....*....
 * $664.598..
 *
 * Task 1: Find the sum of all numbers adjacent to a symbol (everything
 * that is not a '.', '\n', or digit) including diagonally.
 * Test Output: 4361
 *
 * Task 2: If exactly two numbers are next to a '*' they are a gear.
 * Its ratio is the product of the two numbers. Find the sum of all
 * gear ratios.
 * Test Output: 467835
 */
#include <assert.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

typedef uint64_t u64;
typedef uint32_t u32;

typedef struct { void *data; u32 idx, cap; } Vector;
typedef struct { u32 r, c; } Index;
typedef struct { Index s, e; } Pair;

typedef struct {
	int chr;
	char *data;
	u32 dlen;
	u32 pos;
	Index idx;
} Scanner;

static u32 line_length;
static Vector numpairs;
static Vector syms;

static void
die(char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	exit(1);
}

static void
resize_vector(Vector *v, size_t elemsize)
{
	if (v->idx + 1 < v->cap)
		return;

	v->cap += 10000;
	v->data = realloc(v->data, v->cap * elemsize);

	if (!v->data)
		die("realloc\n");
}

static void
inc(Scanner *s)
{
	if (s->chr == '\n') {
		assert(line_length == 0 || line_length == s->idx.c + 1);
		line_length = s->idx.c + 1;
		s->idx.r++;
		s->idx.c = 0;
	} else {
		s->idx.c++;
	}
	s->chr = s->data[++s->pos];
}

static void
dec(Scanner *s)
{
	if (s->pos > 0 && s->data[s->pos - 1] == '\n') {
		s->idx.r--;
		s->idx.c = line_length - 1;
	} else {
		s->idx.c--;
	}
	s->chr = s->data[--s->pos];
}

static void
number(Scanner *s, Vector *v)
{
	resize_vector(v, sizeof(Pair));
	Pair *p = &((Pair *)v->data)[v->idx++];
	p->s = s->idx;
	for (inc(s); s->pos < s->dlen; inc(s))
		if (s->chr < '0' || s->chr > '9')
			break;
	dec(s);
	p->e = s->idx;
}

static void
sym(Scanner *s, Vector *v)
{
	resize_vector(v, sizeof(Index));
	((Index *)v->data)[v->idx++] = s->idx;
}

static void
scan(Scanner *s)
{
	for (; s->pos < s->dlen; inc(s)) {
		if (s->chr >= '0' && s->chr <= '9')
			number(s, &numpairs);
		else if (s->chr != '.' && s->chr != '\n')
			sym(s, &syms);
	}
}

static u64
pair_to_num(Pair *n, char *data)
{
	assert(n->s.r == n->e.r);
	u64 ret = 0;
	u32 len = n->e.c - n->s.c;
	u32 roff = n->s.r * line_length;
	for (u32 i = 0; i <= len; i++) {
		u32 idx = roff + n->s.c + i;
		int t = data[idx] & 0xFF;
		assert(t <= '9' && t >= '0');
		t -= '0';
		for (u32 j = len - i; j > 0; j--)
			t *= 10;
		ret += t;
	}
	return ret;
}

static void
dump_syms(Vector *v)
{
	for (u64 i = 0; i < v->idx; i++) {
		Index s = ((Index *)v->data)[i];
		printf("(%u, %u)\n", s.r, s.c);
	}
}

static void
dump_pairs(Vector *v, char *data)
{
	for (u64 i = 0; i < v->idx; i++) {
		Pair *p = &((Pair *)v->data)[i];
		printf("(%u, %u), (%u, %u) -> %zu\n",
		       p->s.r, p->s.c, p->e.r, p->e.c, pair_to_num(p, data));
	}
}

#define MAX(A, B)               ((A) > (B) ? (A) : (B))
#define MIN(A, B)               ((A) < (B) ? (A) : (B))
static int
intersects(Pair *p, Index *i)
{
	int min = MAX(0, (p->s.c - 1));
	int max = MIN(line_length, p->e.c + 1);
	if (min < 0) min = 0;
	if (min <= i->c && i->c <= max)
		return 1;
	return 0;
}

static int
validate_pair(Pair *p, Vector *v)
{
	Index *si;
	int r1 = p->s.r - 1, r2 = r1 + 1, r3 = r2 + 1;
	int sr = r1 >= 0? r1 : r2;

	u32 idx;
	for (idx = 0; idx < v->idx; idx++) {
		si = &((Index *)v->data)[idx];
		if (si->r >= sr)
			break;
	}
	for (; si->r <= r3 && idx < v->idx; idx++) {
		si = &((Index *)v->data)[idx];
		if (si->r <= r3 && intersects(p, si))
			return 1;
	}
	return 0;
}

static u64
sum_pairs(char *data, Vector *np, Vector *s)
{
	u64 ret = 0;
	for (u32 i = 0; i < np->idx; i++) {
		Pair *p = &((Pair *)np->data)[i];
		if (validate_pair(p, s))
			ret += pair_to_num(p, data);
	}
	return ret;
}

static void
get_stars(Vector *st, Vector *sy, char *data)
{
	for (u32 i = 0; i < sy->idx; i++) {
		Index si = ((Index *)sy->data)[i];
		u32 roff = si.r * line_length;
		int t = data[roff + si.c] & 0xFF;
		if (t != '*')
			continue;
		resize_vector(st, sizeof(Index));
		((Index *)st->data)[st->idx++] = si;
	}
}

static u64
validate_gear(Index *s, Vector *np, char *data)
{
	u64 ret = 0;
	u32 count = 0, idx;
	Pair *p;

	int r1 = s->r - 1, r2 = r1 + 1, r3 = r2 + 1;
	int sr = r1 >= 0? r1 : r2;

	/* skip invalid rows */
	for (idx = 0; idx < np->idx; idx++) {
		p = &((Pair *)np->data)[idx];
		if (p->s.r >= sr)
			break;
	}

	for (; idx < np->idx; idx++) {
		p = &((Pair *)np->data)[idx];
		if (p->s.r > r3)
			break;
		if (!intersects(p, s))
			continue;
		if (ret == 0)
			ret += pair_to_num(p, data);
		else
			ret *= pair_to_num(p, data);
		count++;
		if (count > 2)
			return 0;
	}
	if (count != 2)
		return 0;
	return ret;
}

static u64
gear_ratio(char *data, Vector *np, Vector *s)
{
	Vector stars = {0};
	u64 ret = 0;
	get_stars(&stars, s, data);
	for (u32 i = 0; i < stars.idx; i++) {
		Index *si = &((Index *)stars.data)[i];
		ret += validate_gear(si, np, data);
	}
	free(stars.data);
	return ret;
}

int
main(int argc, char *argv[])
{
	int fd;
	Scanner s = {0};

	if (argc < 2)
		die("must specify a file\n");

	if ((fd = open(argv[1], O_RDONLY)) < 0)
		die("can't open file: %s\n", argv[1]);
	s.dlen = lseek(fd, 0, SEEK_END);
	s.data = mmap(NULL, s.dlen, PROT_READ, MAP_PRIVATE, fd, 0);
	s.chr = s.data[0];
	close(fd);

	scan(&s);
	u64 sum = sum_pairs(s.data, &numpairs, &syms);
	u64 gr = gear_ratio(s.data, &numpairs, &syms);
	printf("Task 1:\t%8zu\n", sum);
	printf("Task 2:\t%8zu\n", gr);

	/* be a good boy and unmap */
	munmap(s.data, s.dlen);

	return 0;
}
