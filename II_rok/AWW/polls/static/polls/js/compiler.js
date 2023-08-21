function showTab(tabId) {
    let tabContents = document.getElementsByClassName("tabcontent");
    for (let i = 0; i < tabContents.length; i++) {
        tabContents[i].style.display = displayNone;
    }
    let tab = document.getElementById(tabId);
    tab.style.display = displayBlock;
    if (tabId === "tab4") {
        let selectedProcessor = document.querySelector('input[name="procesor"]:checked');
        if (selectedProcessor) {
            selectedProcessor = selectedProcessor.value;
        } else {
            selectedProcessor = null;
        }

        let tab4Divs = document.querySelectorAll("#tab4 > div");

        for (let i = 0; i < tab4Divs.length; ++i) {
            if (selectedProcessor == null || selectedProcessor === tab4Divs[i].className) {
                tab4Divs[i].style.display = displayBlock;
            } else {
                tab4Divs[i].style.display = displayNone;
            }
        }
    }
}


function handleSubmitCompile(event) {
    if (selectedFile !== -1) {

        const form = document.getElementById("compileForm");
        console.log(form);
        event.preventDefault();

        const url = 'compile/' + selectedFile + "/"

        const formData = new FormData(form);
        console.log(formData);
        const xhr = new XMLHttpRequest();
        xhr.open('POST', url);
        const csrfToken = form.querySelector('input[name="csrfmiddlewaretoken"]').value;
        xhr.setRequestHeader('X-CSRFToken', csrfToken);

        xhr.onload = function () {
            document.getElementsByTagName("html")[0].innerHTML = xhr.responseText;
        };
        xhr.send(formData);
    } else {
        alert("Wybierz plik");
    }
}


