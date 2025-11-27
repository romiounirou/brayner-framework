interface User {
    id: number;
    username: string;
}

class ApiService {
    endpoint: string = 'https://api.example.com';

    fetchData() {
        var status = 200;
        console.log(status);
    }
}

const api = new ApiService();
api.fetchData();
