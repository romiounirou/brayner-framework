// Test Enhanced Features
var a = 10;
var b = 20;
var c = 0;

// Logical Operators
if (a > 5 && b < 30) {
    console.log("Logical AND works");
}

if (a > 15 || b < 30) {
    console.log("Logical OR works");
}

if (!(a > 20)) {
    console.log("Logical NOT works");
}

// Comparison Operators
if (a < b) {
    console.log("Less than works");
}
if (b > a) {
    console.log("Greater than works");
}
if (a <= 10) {
    console.log("Less than or equal works");
}
if (b >= 20) {
    console.log("Greater than or equal works");
}

// Increment/Decrement
a++;
console.log(a); // Should be 11
b--;
console.log(b); // Should be 19

// Modulus
c = b % 3; // 19 % 3 = 1
console.log(c);

// For Loop
for (let i = 0; i < 5; i++) {
    console.log(i);
}
