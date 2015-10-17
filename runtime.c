#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

struct string {
	int len;
	char c[0];
};

struct string *_alloc_string(int l)
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

int _tiger_ord(struct string *s)
{
	return s->c[0];
}

struct string *chrs[256];

struct string *_tiger_chr(int i)
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

void _tiger_exit(int c)
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

int _tiger_not(int v)
{
	return !v;
}

void _tiger_print_int(int i)
{
	printf("%d", i);
}

int _tiger_size(struct string *s)
{
	return s->len;
}

int _tiger_strcmp(struct string *l, struct string *r)
{
	/* FIXME */
	return 0;
}

int _tiger_streq(struct string *l, struct string *r)
{
	/* FIXME */
	return 0;
}

struct string *_tiger_substring(struct string *s, int from, int to)
{
	/* FIXME */
	return NULL;
}

int tigermain(void);

int main()
{
	int i;

	for (i = 0; i < 256; i++) {
		chrs[i] = _alloc_string(1);
		chrs[i]->c[0] = i;
	}

	return tigermain_0_0();
}
