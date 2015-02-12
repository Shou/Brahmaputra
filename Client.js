
// {{{ Constants

var host = "localhost"
var port = 8080
var path = "/"

var playerSize = 10
var fps = 5
var speed = playerSize / fps

// }}}

// {{{ State

var keyConfig =
    { up:   [87, 38]
    , down: [83, 40]
    , left: [65, 37]
    , right: [68, 39]
    }

var keyboard = []

var player =
    { x: 0
    , y: 0
    }

var ws
var cv
var cx

var moving
var frame

// }}}

// {{{ Utils

// trace :: a -> IO a
function trace(x) { console.log(x); return x }

// }}}

function _move() {
    var frame = playerSize / fps
          , i = 1

    var loop = function() {
        cx.clearRect(0, 0, cv.width, cv.height)

        cx.fillRect( player.x * playerSize + frame * i * x
                   , player.y * playerSize + frame * i * y
                   , playerSize
                   , playerSize
                   )

        if (i < fps) {
            i++
            requestAnimationFrame(loop)

        } else {
            player.x += x
            player.y += y
        }
    }

    requestAnimationFrame(loop)

    //ws.send(x + "," + y)
}

// move :: IO ()
function move() {
    moving = true

    if (frame % fps == 0 && keyboard.length === 0) {
        moving = false
        frame = 0

    } else {
        requestAnimationFrame(move)
    }
}

// keydown :: Event -> IO ()
function keydown(e) {
    keyboard.push(e.keyCode)

    if (! moving) requestAnimationFrame(move)
}

// keyElem :: String -> Int -> Bool
function keyElem(k, e) { return keyConfig[k].indexOf(e) !== -1 }

// keyup :: Event -> IO ()
function keyup(e) {
    for (var i = keyboard.length - 1; i >= 0; i--)
        if (keyboard[i] === e.keyCode) array.splice(i, 1)

    if (! moving) requestAnimationFrame(move)
}

// connect :: String -> Int -> String -> IO WebSocket
function connect(host, port, path, protocol) {
    var ws = new WebSocket("ws://" + host + ':' + port + path)

    ws.onopen = function(_) { ws.send(protocol) }

    return ws
}

// setupCanvas :: Canvas -> IO Context
function setupCanvas(c) {
    c.width = window.innerWidth
    c.height = window.innerHeight
    var cx = c.getContext("2d")

    cx.fillStyle = "#000"

    return cx
}


function main() {
    ws = connect(host, port, path, "coords")
    cv = document.querySelector("canvas")
    cx = setupCanvas(cv)

    document.body.addEventListener("keydown", keydown)

    move(0, 0)
}

