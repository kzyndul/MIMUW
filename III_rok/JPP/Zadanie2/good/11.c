List<int> list(int a, int b) {
    return List<int> [a, b, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3, 3, 8, 3, 2, 7, 9, 5, 0, 2];
}

bool isDivisibleByX(int n, int x) {
    return x * (n / x) == n;
}

bool isPrime(int n) {
    if (n < 2) {
        return false;
    };
    int i = 2;
    while (i < n) {
        if (isDivisibleByX(n, i)) {
            return false;
        };
        i = i + 1;
    };
    return true;
}

int just2() {
    return 2;
}

string hello() {
    return "hello word";
}

int main() {

    print(isPrime(12));
    print("\n");
    print(just2());
    print("\n");
    print(hello());
    print("\n");
    List<int> a = list(3, 1);
    print(a[0]);
    print("\n");
    return 0;
}
