import React, { useState } from 'react';

function UserProfile() {
    const [name, setName] = useState('Alice');

    function updateName() {
        var newName = "Bob";
        console.log(newName);
    }

    return (
        <div>
            <h1>{name}</h1>
            <button onClick={updateName}>Update</button>
        </div>
    );
}

export default UserProfile;
