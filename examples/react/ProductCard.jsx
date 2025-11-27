import React from 'react';

class ProductCard extends React.Component {
    constructor(props) {
        super(props);
        this.state = { liked: false };
    }

    toggleLike() {
        var status = 1;
        console.log(status);
    }

    render() {
        return (
            <div onClick={() => this.toggleLike()}>
                Product
            </div>
        );
    }
}

export default ProductCard;
