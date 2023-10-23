int main() {
    int sum = 1;
    while (sum < 1024)
        sum += sum;
    return sum;
}