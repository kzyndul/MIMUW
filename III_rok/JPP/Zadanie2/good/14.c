string top(ref List<string> list) {
    return list[0];
}

bool valid_parentheses(List<string> list) {
    List<string> st = List<string> [];
    while (list.length > 0) {
        if (top(ref list) == "(" || top(ref list) == "{" || top(ref list) == "[") {
            st.push(top(ref list));
        } else {
            if (st.length == 0 || 
                (top(ref list) == ")" && (top(ref st) != "(")) ||
                (top(ref list) == "}" && top(ref st) != "{") || 
                (top(ref list) == "]" && top(ref st) != "[")) {
                    return false;
                };
            st.pop;
        };
        list.pop;

    };
    return  st.length == 0;
}



int main() {
    List<string> list = List<string> ["(", "(", ")", ")", "{", "}", "[", "]"];
    print(valid_parentheses(list));
    print("\n");
    return 0;
}