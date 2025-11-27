export function calculateTotal(price: number, tax: number): number {
    return price + tax;
}

export function formatName(first: string, last: string): string {
    return first + " " + last;
}

const result: number = calculateTotal(100, 20);
console.log(result);
