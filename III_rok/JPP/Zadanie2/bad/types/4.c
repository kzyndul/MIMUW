int return1() {
    return 1;
}


int main() {
    List<string> a = List<string>["aaa", "bbb"];
    a.push(return1());
    return 2;
}