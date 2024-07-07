int main() {

    int test(ref int a) {
        return a;
    };

    int a = 5;
    test(a);
    return 0;
}
