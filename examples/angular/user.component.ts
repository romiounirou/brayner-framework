import { Component } from '@angular/core';

@Component({
    selector: 'app-user',
    template: '<div>User Profile</div>'
})
export class UserComponent {
    username = 'JohnDoe';
    age = 30;

    updateProfile() {
        this.age = this.age + 1;
        console.log(this.age);
    }
}
