let sprites: { [key: string]: { [key: string]: HTMLImageElement } } = {
    red: load_colored_images(new Color(.7, 0, 0)),
    green: load_colored_images(new Color(.3, 1, .3)),
    blue: load_colored_images(new Color(0, 0, .7)),
    yellow: load_colored_images(new Color(1, 1, 0))
};

function load_colored_images(color: Color): { [key: string]: HTMLImageElement } {
    let obj: { [key: string]: HTMLImageElement } = {};
    for (let piece of ['bishop', 'king', 'knight', 'pawn', 'queen', 'rook']) {
        let image = new Image();
        image.src = 'img/' + piece + '_white.png';
        image.onload = function() {
            image.onload = function() { };
            tintImage(image, color);
            //            image
            //            ctx.save();
            //            ctx.drawImage(image, 0, 0);
            //            ctx.globalCompositeOperation = 'multiply';
            //            ctx.fillStyle = color;
            //            ctx.fillRect(0, 0, canvas.width, canvas.height);
            //            ctx.restore();
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

