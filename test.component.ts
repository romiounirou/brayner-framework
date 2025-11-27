import { Component } from '@angular/core';

@Component({
    selector: 'app-root',
    templateUrl: './app.component.html'
})
export class AppComponent {
    title = 'brayner-angular';

    changeTitle() {
        var newVal = 100;
        console.log(newVal);
    }

    ngOnInit() {
        this.changeTitle();
    }
}
