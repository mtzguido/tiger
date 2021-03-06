#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

struct string {
	long len;
	char c[0];
};

struct string *_alloc_string(long l)
{
	struct string *ret = malloc(offsetof(struct string, c[l]));

	if (!ret) {
		fprintf(stderr, "Runtime OOM\n");
		abort();
	}

	ret->len = l;
	return ret;
}

void _tiger_print_file(struct string *s, FILE *f)
{
	int i;

	for (i = 0; i < s->len; i++)
		putc(s->c[i], f);
}

void _tiger_print(struct string *s)
{
	_tiger_print_file(s, stdout);
}

void _tiger_print_err(struct string *s)
{
	_tiger_print_file(s, stderr);
}

long _tiger_ord(struct string *s)
{
	return s->c[0];
}

struct string *chrs[256];

struct string *_tiger_chr(long i)
{
	return chrs[i];
}

struct string *_tiger_concat(struct string *l, struct string *r)
{
	struct string *ret = _alloc_string(l->len + r->len);

	memcpy(ret->c, l->c, l->len);
	memcpy(ret->c + l->len, r->c, r->len);

	return ret;
}

void _tiger_exit(long c)
{
	exit(c);
}

void _tiger_flush(void)
{
	fflush(NULL);
}

struct string *_tiger_getchar(void)
{
	struct string *ret = _alloc_string(1);
	ret->c[0] = getchar();
	return ret;
}

long _tiger_not(long v)
{
	return !v;
}

void _tiger_print_int(long i)
{
	printf("%ld", i);
}

long _tiger_size(struct string *s)
{
	return s->len;
}

long _tiger_strcmp(struct string *l, struct string *r)
{
	int i;

	for (i = 0; i < l->len && i < r->len; i++) {
		if (l->c[i] < r->c[i])
			return -1;
		else if (l->c[i] > r->c[i])
			return 1;
	}

	if (l -> len > r->len)
		return -1;
	else if (l -> len < r->len)
		return 1;

	return 0;
}

struct string *_tiger_substring(struct string *s, long from, long len)
{
	struct string *ret = _alloc_string(len);

	memcpy(ret->c, s->c + from, len);
	return ret;
}

void *__mk_array(long init, long size)
{
	int i;
	long *ret = malloc(size * sizeof ret[0]);

	for (i = 0; i < size; i++)
		ret[i] = init;

	return ret;
}

void *__mk_record(long len, ...)
{
	long *ret;
	va_list l;
	int i;

	ret = malloc(len * sizeof ret[0]);

	va_start(l, len);

	for (i = 0; i < len; i++)
		ret[i] = va_arg(l, long);

	va_end(l);

	return ret;
}

long _tigermain_0_0(void);

int main()
{
	int i;

	for (i = 0; i < 256; i++) {
		chrs[i] = _alloc_string(1);
		chrs[i]->c[0] = i;
	}

	return _tigermain_0_0();
}
