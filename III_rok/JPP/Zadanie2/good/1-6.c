int printSpaces(int num) {
	int i = 0;
	while(i < num) {
		print(" ");
		i = i + 1;
	};
	return 0;
}

int printStars(int num) {
	if (num > 0) {
		int tmp = printStars(num - 1);
		print("*");
	};
	return 0;
}

int printLayer(int space, int star) {
    int tmp1 = printSpaces(space);
    int tmp2 = printStars(star);
    print("\n");
	return 0;
}


int printTreeLayer(int height) {
	int i = 0;
	while(i <= height) {
		int tmp = printLayer(height - i, 2 * i + 1);
		i = i + 1;
	};
	return 0;
}

int printTrunk(int treeHeight, int width, int height) {
    int i = 0;
    while(i < height) {
        int tmp = printLayer(treeHeight - (width), 2 * width + 1);
        i = i + 1;
    };
    return 0;
}

int printTree(int treeHeight, int trunkWidth, int trunkHeight) {
	int tmp1 = printTreeLayer(treeHeight);
	int tmp2 = printTrunk(treeHeight, trunkWidth, trunkHeight);
	return 0;
}

int main() {	
	return printTree(9, 2, 4);
}

