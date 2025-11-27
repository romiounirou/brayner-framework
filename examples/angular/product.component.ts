import { Component } from '@angular/core';

@Component({
    selector: 'app-product',
    template: '<div>Product Details</div>'
})
export class ProductComponent {
    productName = 'Laptop';
    price = 999;

    applyDiscount() {
        this.price = this.price * 0.9;
        console.log(this.price);
    }
}
