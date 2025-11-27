import React from 'react';

function App() {
    const title = "Hello React";
    const count = 10;

    function increment() {
        const newVal = count + 1;
        console.log(newVal);
    }

    increment();

    return <div>{title}</div>;
}
