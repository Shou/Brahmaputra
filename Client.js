
// {{{ Constants

// host, path :: String
var host = "localhost"
var path = "/"
// port :: Int
var port = 8080

// playerSize, maxSpeed :: Int
var playerSize = 10
var maxSpeed = 2
// speedStep :: Float
var speedStep = maxSpeed / 10

// key :: String
var key

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

// | Player coordinate websocket
// cows :: WebSocket
var cows
// | Chat websocket
// chws :: WebSocket
var chws
// | Mood websocket
// mows :: WebSocket
var mows

// cv :: Canvas
var cv
// cx :: CanvasContext
var cx

var speedX = 0
var speedY = 0

var chat

// }}}

// {{{ Utils

// trace :: a -> IO a
function trace(x) { console.log(x); return x }

// log :: a -> IO ()
function log(x) { console.log(x) }

// | Generate a random string (a-z0-9) of length n
// randomString :: Int -> String
function randomString(n) {
    var str = ""
    while (str.length < n)
        str += Math.random().toString(36).substr(2, n - str.length)

    return str
}

// }}}

// chatSend :: String -> IO ()
function chatSend(m) {
    chws.send(m)

    // TODO hide on successful post
    //chat.classList.add("hide")
    chat.value = ""
}

// showChat :: IO ()
function showChat() {
    if (! chat) {
        chat = document.createElement("input")

        chat.addEventListener("keypress", function(e) {
            e.stopPropagation()

            if (e.keyCode === 13) chatSend(this.value)
        })

        document.body.appendChild(chat)
    }

    chat.classList.remove("hide")

    chat.focus()
}

// TODO slow down movement when keyboard is empty, towards even pixel number.
// move :: IO ()
function move() {
    var next = requestAnimationFrame(move)
    moving = true

    // FIXME right/down accelerates left/up to insane speeds when used after
    if (keyElem("right"))
        speedX = Math.min((speedX ? speedX : speedStep) * 1.2, maxSpeed)
    if (keyElem("left"))
        speedX = Math.max( Math.abs((speedX ? speedX : speedStep)) * -1.2
                         , -maxSpeed
                         )
    if (keyElem("down"))
        speedY = Math.min((speedY ? speedY : speedStep) * 1.2, maxSpeed)
    if (keyElem("up"))
        speedY = Math.max( Math.abs((speedY ? speedY : speedStep)) * -1.2
                         , -maxSpeed
                         )

    player.x += speedX
    player.y += speedY

    log(player.x + " x " + player.y + ": " + speedX + " * " + speedY)

    cx.clearRect(0, 0, cv.width, cv.height)
    cx.fillRect( player.x
               , player.y
               , playerSize
               , playerSize
               )

    var emptyKeyboard = keyboard.length === 0
    var evenX = Math.round(player.x) % playerSize === 0
    var evenY = Math.round(player.y) % playerSize === 0

    if (emptyKeyboard && evenX && evenY) {
        speedX = speedY = 0
        cancelAnimationFrame(next)
        moving = false

    } else if (emptyKeyboard && evenX) speedX = 0

    else if (emptyKeyboard && evenY) speedY = 0
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

// connect :: String -> Int -> String -> String -> IO WebSocket
function connect(host, port, path, protocol, key) {
    var ws = new WebSocket("ws://" + host + ':' + port + path)

    ws.onopen = function(_) { ws.send(protocol + " " + key) }
    ws.onclose = function(_) { ws.close() }
    ws.onmessage = function(m) { console.log(m.data) }

    return ws
}

// setupCanvas :: Canvas -> IO Context
function setupCanvas(c) {
    c.width = window.innerWidth
    c.height = window.innerHeight
    var cx = c.getContext("2d")

    cx.fillStyle = "#FFF"

    return cx
}


function main() {
    key = randomString(256)

    mvws = connect(host, port, path, "move", key)
    chws = connect(host, port, path, "chat", key)
    mows = connect(host, port, path, "mood", key)
    cv = document.querySelector("canvas")
    cx = setupCanvas(cv)

    document.body.addEventListener("keydown", keydown)
    document.body.addEventListener("keyup", keyup)

    move(0, 0)
}

