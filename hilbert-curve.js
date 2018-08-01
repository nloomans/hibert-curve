function pointEq(a, b) {
    return a[0] === b[0] && a[1] === b[1];
}

function flipLeft(point) {
    if (pointEq(point, [0, 0])) return [1, 1];
    if (pointEq(point, [1, 0])) return point;
    if (pointEq(point, [1, 1])) return [0, 0];
    if (pointEq(point, [0, 1])) return point;
    throw new Error(`Invalid point! ${point}`);
}

function flipRight(point) {
    if (pointEq(point, [0, 0])) return point;
    if (pointEq(point, [0, 1])) return [1, 0];
    if (pointEq(point, [1, 1])) return point;
    if (pointEq(point, [1, 0])) return [0, 1];
    throw new Error(`Invalid point! ${point}`);
}

function mapPoint(point, quadrant, lineSize) {
    const [pointX, pointY] = point;

    // Top left
    if (pointEq(quadrant, [0, 0])) {
        return point;
    }

    // Bottom left
    if (pointEq(quadrant, [1, 0])) {
        return [pointX + lineSize, pointY];
    }

    // Top right
    if (pointEq(quadrant, [0, 1])) {
        return [pointX, pointY + lineSize];
    }

    // Bottom left
    if (pointEq(quadrant, [1, 1])) {
        return [pointX + lineSize, pointY + lineSize];
    }
}

function hilbertCurve(order, point1d) {
    const size = Math.pow(4, order);
    const lineSize = Math.pow(2, order - 1);
    const quadrantSize = size / 4;
    
    if (order === 1) {
        if (point1d < quadrantSize) {
            // Bottom left
            return [1, 0];
        } else if (point1d < quadrantSize * 2) {
            // Top left
            return [0, 0];
        } else if (point1d < quadrantSize * 3) {
            // Top Right
            return [0, 1];
        } else if (point1d < quadrantSize * 4) {
            // Bottom right
            return [1, 1];
        } else {
            throw new Error(`Point ${point1d} is not smaller than size ${size}`);
        }
    } else {
        if (point1d < quadrantSize) {
            // Bottom left
            const point = hilbertCurve(order - 1,  point1d);
            return mapPoint(flipLeft(point), [1, 0], lineSize);
        } else if (point1d < quadrantSize * 2) {
            // Top left
            const point = hilbertCurve(order - 1, point1d - (quadrantSize * 1));
            return mapPoint(point, [0, 0], lineSize);
        } else if (point1d < quadrantSize * 3) {
            // Top Right
            const point = hilbertCurve(order - 1, point1d - (quadrantSize * 2));
            return mapPoint(point, [0, 1], lineSize);
        } else if (point1d < quadrantSize * 4) {
            // Bottom right
            const point = hilbertCurve(order - 1, point1d - (quadrantSize * 3));
            return mapPoint(flipRight(point), [1, 1], lineSize);
        } else {
            throw new Error(`Point ${point1d} is not smaller than size ${size}`);
        }
    }
}

for (let i = 0; i < 16; i++) {
    console.log(`2D (${hilbertCurve(2, i)}) <-> 1D ${i+1}`);
}
