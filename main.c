#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define N 256

const uint64_t kHashStart = UINT64_C(14695981039346656037);
const uint64_t kHashMul = UINT64_C(1099511628211);

struct Symbol {
    int c;
    uint64_t cnt;
};

struct Code {
    int len;
    unsigned char bits[N];
};

struct Node {
    int to[2];
    int c;
};

struct Bits {
    FILE* f;
    int cur, cnt;
};

struct Symbol a[N];
struct Code codes[N];
struct Node t[2 * N];
int nodes;

int Cmp(const void* x, const void* y) {
    const struct Symbol* p = x;
    const struct Symbol* q = y;
    if (p->cnt != q->cnt) {
        return p->cnt > q->cnt ? -1 : 1;
    }
    return p->c - q->c;
}

int GetSymbols(uint64_t freq[N]) {
    int n = 0;
    for (int i = 0; i < N; i++) {
        if (freq[i] == 0) {
            continue;
        }
        a[n].c = i;
        a[n].cnt = freq[i];
        n++;
    }
    qsort(a, (size_t)n, sizeof(a[0]), Cmp);
    return n;
}

int Build(int l, int r, unsigned char path[N], int len) {
    int v = ++nodes;
    if (l == r) {
        int c = a[l].c;
        t[v].c = c + 1;
        codes[c].len = len;
        memcpy(codes[c].bits, path, (size_t)len);
        return v;
    }

    uint64_t sum = 0, cur = 0, best = UINT64_MAX;
    for (int i = l; i <= r; i++) {
        sum += a[i].cnt;
    }

    int mid = l;
    for (int i = l; i < r; i++) {
        cur += a[i].cnt;
        uint64_t diff = cur > sum - cur ? cur - (sum - cur) : (sum - cur) - cur;
        if (diff < best) {
            best = diff;
            mid = i;
        }
    }

    path[len] = 0;
    t[v].to[0] = Build(l, mid, path, len + 1);
    path[len] = 1;
    t[v].to[1] = Build(mid + 1, r, path, len + 1);
    return v;
}

void MakeCodes(int n) {
    if (n == 0) {
        return;
    }
    unsigned char path[N] = {0};
    nodes = 0;
    if (n == 1) {
        nodes = 1;
        t[1].to[0] = Build(0, 0, path, 1);
    } else {
        Build(0, n - 1, path, 0);
    }
}

int WriteNum(FILE* f, uint64_t x, int n) {
    for (int i = 0; i < n; i++) {
        if (fputc((x >> (8 * i)) & 255, f) == EOF) {
            return 0;
        }
    }
    return 1;
}

int ReadNum(FILE* f, uint64_t* x, int n) {
    *x = 0;
    for (int i = 0; i < n; i++) {
        int c = fgetc(f);
        if (c == EOF) {
            return 0;
        }
        *x |= (uint64_t)c << (8 * i);
    }
    return 1;
}

int WriteBit(struct Bits* b, int x) {
    b->cur = (b->cur << 1) | x;
    b->cnt++;
    if (b->cnt == 8) {
        if (fputc(b->cur, b->f) == EOF) {
            return 0;
        }
        b->cur = 0;
        b->cnt = 0;
    }
    return 1;
}

int ReadBit(struct Bits* b) {
    if (b->cnt == 0) {
        b->cur = fgetc(b->f);
        if (b->cur == EOF) {
            return -1;
        }
        b->cnt = 8;
    }
    b->cnt--;
    return (b->cur >> b->cnt) & 1;
}

uint64_t AddHash(uint64_t h, int c) {
    return (h ^ (uint64_t)c) * kHashMul;
}

int Compress(const char* input, const char* output) {
    FILE* in = fopen(input, "rb");
    if (in == NULL) {
        perror("Cannot open input file");
        return 1;
    }

    uint64_t freq[N] = {0};
    uint64_t sz = 0, hash = kHashStart;
    int c;
    while ((c = fgetc(in)) != EOF) {
        if (sz == UINT64_MAX) {
            break;
        }
        freq[c]++;
        sz++;
        hash = AddHash(hash, c);
    }
    if (ferror(in) || sz == UINT64_MAX || fseek(in, 0, SEEK_SET) != 0) {
        fprintf(stderr, "Cannot read input file\n");
        fclose(in);
        return 1;
    }

    int n = GetSymbols(freq);
    MakeCodes(n);
    FILE* out = fopen(output, "wb");
    if (out == NULL) {
        perror("Cannot open output file");
        fclose(in);
        return 1;
    }

    int ok = fwrite("SF01", 1, 4, out) == 4 && WriteNum(out, sz, 8) && WriteNum(out, hash, 8) &&
             WriteNum(out, (uint64_t)n, 2);
    for (int i = 0; i < n && ok; i++) {
        ok = fputc(a[i].c, out) != EOF && WriteNum(out, a[i].cnt, 8);
    }

    struct Bits b = {out, 0, 0};
    while (ok && (c = fgetc(in)) != EOF) {
        for (int j = 0; j < codes[c].len && ok; j++) {
            ok = WriteBit(&b, codes[c].bits[j]);
        }
    }
    if (ferror(in)) {
        ok = 0;
    }
    if (ok && b.cnt != 0) {
        ok = fputc(b.cur << (8 - b.cnt), out) != EOF;
    }
    if (fclose(in) != 0) {
        ok = 0;
    }
    if (fclose(out) != 0) {
        ok = 0;
    }

    if (!ok) {
        fprintf(stderr, "Cannot write compressed file\n");
        remove(output);
        return 1;
    }
    return 0;
}

int Decompress(const char* input, const char* output) {
    FILE* in = fopen(input, "rb");
    if (in == NULL) {
        perror("Cannot open input file");
        return 1;
    }

    char magic[4];
    uint64_t sz = 0, hash = 0, cnt = 0, sum = 0;
    uint64_t freq[N] = {0};
    int ok = fread(magic, 1, 4, in) == 4 && memcmp(magic, "SF01", 4) == 0 && ReadNum(in, &sz, 8) &&
             ReadNum(in, &hash, 8) && ReadNum(in, &cnt, 2) && cnt <= N;

    for (int i = 0; i < (int)cnt && ok; i++) {
        int c = fgetc(in);
        uint64_t x = 0;
        if (c == EOF || !ReadNum(in, &x, 8) || x == 0 || freq[c] != 0 || x > sz - sum) {
            ok = 0;
        } else {
            freq[c] = x;
            sum += x;
        }
    }
    if (sum != sz || (cnt == 0) != (sz == 0)) {
        ok = 0;
    }
    if (!ok) {
        fprintf(stderr, "Invalid compressed file format\n");
        fclose(in);
        return 1;
    }

    int n = GetSymbols(freq);
    MakeCodes(n);
    FILE* out = fopen(output, "wb");
    if (out == NULL) {
        perror("Cannot open output file");
        fclose(in);
        return 1;
    }

    struct Bits b = {in, 0, 0};
    uint64_t cur_hash = kHashStart;
    for (uint64_t i = 0; i < sz && ok; i++) {
        int v = 1;
        while (t[v].c == 0) {
            int bit = ReadBit(&b);
            if (bit == -1 || t[v].to[bit] == 0) {
                ok = 0;
                break;
            }
            v = t[v].to[bit];
        }
        if (!ok) {
            break;
        }
        int c = t[v].c - 1;
        if (freq[c] == 0 || fputc(c, out) == EOF) {
            ok = 0;
        } else {
            freq[c]--;
            cur_hash = AddHash(cur_hash, c);
        }
    }

    if (cur_hash != hash) {
        ok = 0;
    }
    if (b.cnt != 0 && (b.cur & ((1 << b.cnt) - 1)) != 0) {
        ok = 0;
    }
    if (fgetc(in) != EOF || ferror(in)) {
        ok = 0;
    }
    for (int i = 0; i < N; i++) {
        if (freq[i] != 0) {
            ok = 0;
        }
    }

    if (fclose(in) != 0) {
        ok = 0;
    }
    if (fclose(out) != 0) {
        ok = 0;
    }
    if (!ok) {
        fprintf(stderr, "Invalid compressed file format or I/O error\n");
        remove(output);
        return 1;
    }
    return 0;
}

int main(int argc, char** argv) {
    if (argc != 4 || (strcmp(argv[1], "-c") != 0 && strcmp(argv[1], "-d") != 0)) {
        fprintf(stderr, "Usage: %s [-c|-d] input_file output_file\n", argv[0]);
        return 1;
    }
    if (strcmp(argv[2], argv[3]) == 0) {
        fprintf(stderr, "Input and output files must be different\n");
        return 1;
    }
    if (strcmp(argv[1], "-c") == 0) {
        return Compress(argv[2], argv[3]);
    }
    return Decompress(argv[2], argv[3]);
}
