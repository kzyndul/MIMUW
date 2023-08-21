function hideSectionInformation(button) {
    if (selectedFile !== -1) {
        const section = button.parentElement;
        const sectionInfo = section.querySelector('.section_information');
        if (sectionInfo.style.display === displayNone) {
            sectionInfo.style.display = displayBlock;
            button.textContent = buttonHiden;
        } else {
            sectionInfo.style.display = displayNone;
            button.textContent = buttonShown;
        }
    }
}


function hideAllSectionInformation(button) {
    if (selectedFile !== -1) {

        const sectionInfo = document.querySelectorAll('.section_information');
        if (!isSectionHide) {
            sectionInfo.forEach(info => {
                info.style.display = displayNone;
            });
        } else {
            sectionInfo.forEach(info => {
                info.style.display = displayBlock;
            });
        }

        if (!isSectionHide) {
            button.textContent = buttonShown;
        } else {
            button.textContent = buttonHiden;
        }

        const buttons = document.querySelectorAll('.fileSectionButtonHide');

        if (!isSectionHide) {
            buttons.forEach(button => {
                button.textContent = buttonShown;
            });
        } else {
            buttons.forEach(button => {
                button.textContent = buttonHiden;
            });
        }
        isSectionHide = !isSectionHide;
    }
}


function deleteFileSection(flieSectionPK) {

    const xhr = new XMLHttpRequest();
    const url = 'deleteSection/' + selectedFile + "/" + flieSectionPK + "/"

    xhr.open('GET', url);
    xhr.onload = function () {
        document.getElementsByTagName("html")[0].innerHTML = xhr.responseText;
    };
    xhr.send();
}


function highlightSection(begin, end) {
    const divs = document.querySelectorAll('.glowneOkno div');
    for (let i = begin - 1; i < end; ++i) {
        if (divs[i].style.background === highlightColor) {
            divs[i].style.background = displayNone;
        } else {
            divs[i].style.background = highlightColor;
        }
    }

}


function showFileSectionCreate() {
    if (selectedFile !== -1) {
        selectedText = window.getSelection().toString();
        const div = document.getElementsByClassName("createFileSection")[0];
        div.style.display = displayBlock;
    } else {
        alert("Wybierz plik");
    }
}

function handleSubmitFileSection(event) {

    const form = document.querySelector('#fileSectionForm');
    event.preventDefault();
    if (selectedFile !== -1 && selectedText !== "") {
        console.log(selectedText);
        document.querySelector('#selectedTextFileSection').value = selectedText;

        const formData = new FormData(form);

        const xhr = new XMLHttpRequest();
        xhr.open('POST', 'createSection/' + selectedFile + "/");
        const csrfToken = form.querySelector('input[name="csrfmiddlewaretoken"]').value;
        xhr.setRequestHeader('X-CSRFToken', csrfToken);
        xhr.onload = function () {
            document.getElementsByTagName("html")[0].innerHTML = xhr.responseText;
        };
        xhr.send(formData);
    } else if (selectedText === "") {
        alert("Zaznacze treść sekcji");
    } else {
        alert("Wybierz plik");
    }
    selectedText = "";
}



