
// {{{ Constants

var host = "localhost"
var port = 8080
var path = "/"

var playerSize = 10
var fps = 5
var speed = playerSize / fps

// }}}

// {{{ State

// keyConfig :: Object String [Int]
var keyConfig =
    { up:   [87, 38]
    , down: [83, 40]
    , left: [65, 37]
    , right: [68, 39]
    }

// keyboard :: [Int]
var keyboard = []

// player :: Object String Int
var player =
    { x: 0
    , y: 0
    }

// | Player movement websocket
// ws :: WebSocket
var mws
// | Chat websocket
// ws :: WebSocket
var cws
// cv :: Canvas
var cv
// cx :: CanvasContext
var cx

// moving :: Bool
var moving = false
// frame :: Int
var frame = 1

// }}}

// {{{ Utils

// trace :: a -> IO a
function trace(x) { console.log(x); return x }

// log :: a -> IO ()
function log(x) { console.log(x) }

// }}}

// TODO rAF should be at the beginning of the code
// FIXME movement too fast; no acceleration limits.
// move :: IO ()
function move() {
    moving = true

    if (keyElem("right"))
        player.x = Math.min(player.x + speed * frame, speed * frame * 2)
    if (keyElem("left"))
        player.x = Math.max(player.x - speed * frame, speed * frame * -2)
    if (keyElem("down"))
        player.y = Math.min(player.y + speed * frame, speed * frame * 2)
    if (keyElem("up"))
        player.y = Math.max(player.y - speed * frame, speed * frame * -2)

    log(player.x + " x " + player.y)

    cx.clearRect(0, 0, cv.width, cv.height)
    cx.fillRect( player.x
               , player.y
               , playerSize
               , playerSize
               )

    if (frame % fps == 0 && keyboard.length === 0) {
        moving = false
        frame = 1

    } else {
        frame++
        requestAnimationFrame(move)
    }
}

// keyElem :: String -> Int -> Bool
function keyElem(k, e) {
    for (var i = 0; i < keyboard.length; i++)
        if (keyConfig[k].indexOf(keyboard[i]) !== -1) return true
}

// keydown :: Event -> IO ()
function keydown(e) {
    if (keyboard.indexOf(e.keyCode) === -1)
        keyboard.push(e.keyCode)

    if (e.keyCode === 13) showChat()

    else if (! moving) requestAnimationFrame(move)

    console.log(keyboard)
}

// keyup :: Event -> IO ()
function keyup(e) {
    for (var i = keyboard.length - 1; i >= 0; i--)
        if (keyboard[i] === e.keyCode) keyboard.splice(i, 1)

    if (! moving) requestAnimationFrame(move)

    console.log(keyboard)
}

// connect :: String -> Int -> String -> IO WebSocket
function connect(host, port, path, protocol) {
    var ws = new WebSocket("ws://" + host + ':' + port + path)

    ws.onopen = function(_) { ws.send(protocol) }
    ws.onclose = function(_) { delete ws }

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
    document.body.addEventListener("keyup", keyup)

    move(0, 0)
}

