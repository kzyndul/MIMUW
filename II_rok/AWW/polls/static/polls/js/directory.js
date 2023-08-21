function showDirectoryCreate() {
    const div = document.getElementsByClassName("createDirectory")[0];
    div.style.display = displayBlock;
}


function handleSubmitDirectory(event) {
    const form = document.querySelector('#directoryForm');
    event.preventDefault();

    const formData = new FormData(form);

    const xhr = new XMLHttpRequest();
    xhr.open('POST', 'getHomePageDirectory/');
    const csrfToken = form.querySelector('input[name="csrfmiddlewaretoken"]').value;
    xhr.setRequestHeader('X-CSRFToken', csrfToken);

    xhr.onload = function () {
        document.getElementsByTagName("html")[0].innerHTML = xhr.responseText;
    };
    xhr.send(formData);
}



function selectDirectory(id) {
    selectedFile = -1;
    selectedDirectory = id;

    const xhr = new XMLHttpRequest();

    xhr.open('GET', "getHomePageDirectory");
    xhr.onload = function () {
        document.getElementsByTagName("html")[0].innerHTML = xhr.responseText;
    };
     xhr.send();
}

function deleteDirectory() {
    if (selectedDirectory !== -1) {
        const xhr = new XMLHttpRequest();
        const url = 'deleteDirectory/' + selectedDirectory + "/"

        xhr.open('GET', url);
        xhr.onload = function () {
            document.getElementsByTagName("html")[0].innerHTML = xhr.responseText;
        };
        xhr.send();
        selectedDirectory = -1
    } else {
        alert("Wybierz folder")
    }
}