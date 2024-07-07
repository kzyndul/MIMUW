int withoutBreak() {
    int i = 0;
    int a = 3;
    while(i < 5) {
        int a = 5;
        i = i + 1;
    };
    return a;
}

int withBreak() {
    int i = 0;
    while(i < 10) {
        if (i == 5) {
            break;
        };
        i = i + 1;
    };
    return i;
}

int withContinue() {
    int i = 0;
    while(i < 10) {
        if (i < 6) {
            i = i + 1;
            continue;
        };
        return i;
    };
}

int withReturn() {
    int i = 0;
    while(i < 10) {
        if (i == 5) {
            return i;
        };
        i = i + 1;
    };
    return i;
}

int main() {
    print(withoutBreak());
    print("\n");
    print(withReturn());
    print("\n");
    print(withBreak());
    print("\n");
    print(withContinue());
    print("\n");
    return 0;
}
