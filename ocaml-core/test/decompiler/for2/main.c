int main() {
    int sum = 0;
    for (int i = -5; i < 5; i++) {
        for (int j = -5; j < 5; j++) {
            sum += i * j;
        }
    }
    return sum;
}