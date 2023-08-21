function showFlieCreate() {
    const div = document.getElementsByClassName("createFile")[0];
    div.style.display = displayBlock;
}


function handleSubmitFile(event) {
    const form = document.querySelector('#fileForm');
    event.preventDefault();

    const formData = new FormData(form);

    const xhr = new XMLHttpRequest();
    xhr.open('POST', 'getHomePageFile/0/');
    const csrfToken = form.querySelector('input[name="csrfmiddlewaretoken"]').value;
    xhr.setRequestHeader('X-CSRFToken', csrfToken);

    xhr.onload = function () {
        document.getElementsByTagName("html")[0].innerHTML = xhr.responseText;
    };
    xhr.send(formData);
}

function selectFile(id) {
    selectedFile = id;
    selectedDirectory = -1;
    const xhr = new XMLHttpRequest();
    let url = 'getHomePageFile/' + id + "/"

    xhr.open('GET', url);
    xhr.onload = function () {
        document.getElementsByTagName("html")[0].innerHTML = xhr.responseText;
    };
    xhr.send();
}


function deleteFile() {
    if (selectedFile !== -1) {
        const xhr = new XMLHttpRequest();
        const url = 'deleteFile/' + selectedFile + "/"

        xhr.open('GET', url);
        xhr.onload = function () {
            document.getElementsByTagName("html")[0].innerHTML = xhr.responseText;
        };
        xhr.send();
        selectedFile = -1
    } else {
        alert("Wybierz plik")
    }
}
