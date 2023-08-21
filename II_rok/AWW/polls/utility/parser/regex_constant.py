line_comment = r"\/\/"
line_comment_cut = r'\/\/.*'
block_comment_start = r"\/\*"
block_comment_end = r"\*\/"
curly_braces_open = r"\{"
curly_braces_close = r"\}"
block_compiler_directive_start = r"#if"
block_compiler_directive_end = r"#endif"
line_compiler_directive = r"#(define|include)"


one_new_line = r'\n+'


assembler_start = r"__asm"
assembler_end = r"__endasm"


variable = r"\b(?:(?:auto\s*|const\s*|unsigned\s*|__sfr __at\(.*\)\s*|signed\s*|register\s*|volatile bool\s*|volatile\s*|volatile static bool\s*|volatile static clock_t\s*|short\s*|long\s*|char\s*|int\s*|float\s*|double\s*|_Bool\s*|complex\s*)+)(?:\s+\*?\*?\s*)([a-zA-Z_][a-zA-Z0-9_]*)\b(?<!clock_t)\s*[\[;,=)]"

function = r"(void|signed char|char|unsigned char|short|short int|signed short|signed short|int|unsigned short|unsigned short|int|int|signed|signed int|unsigned|unsigned int|long|long int|signed long|signed long int|unsigned long|unsigned long int|long long|long long int|signed long long|signed long long int|unsigned long long|unsigned long long int|float|double|long double)\s+[a-zA-Z_][a-zA-Z0-9_]*\(.*\)"

line_number = r":(\d+): (warning|.*error|.*Error)"

