let sprites: { [key: string]: { [key: string]: HTMLImageElement } } = {
    Red: loadColoredImages(new Color(.7, 0, 0)),
    Green: loadColoredImages(new Color(.6, 1, .6)),
    Blue: loadColoredImages(new Color(.2, .2, .7)),
    Yellow: loadColoredImages(new Color(1, 1, .5))
};

function loadColoredImages(color: Color): { [key: string]: HTMLImageElement } {
    let obj: { [key: string]: HTMLImageElement } = {};
    for (let piece of ['Bishop', 'King', 'Knight', 'Pawn', 'Queen', 'Rook']) {
        let image = new Image();
        image.src = 'img/' + piece + '_white.png';
        image.onload = function() {
            image.onload = function() { };
            tintImage(image, color);
        };
        obj[piece] = image;
    }
    return obj;
}

function tintImage(imgElement: HTMLImageElement, color: Color) {
    // create hidden canvas (using image dimensions)
    let canvas = document.createElement('canvas');
    canvas.width = imgElement.width;
    canvas.height = imgElement.height;

    let ctx = canvas.getContext('2d');
    ctx.drawImage(imgElement, 0, 0);

    let imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    let data = imageData.data;

    for (let p = 0; p < data.length; p += 4) {
        data[p] *= color.r;
        data[p + 1] *= color.g;
        data[p + 2] *= color.b;
    }

    ctx.putImageData(imageData, 0, 0);
    imgElement.src = canvas.toDataURL();
}

