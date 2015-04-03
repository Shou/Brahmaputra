
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

var stepX = 0
var stepY = 0

var playerColor = "white"

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
    if (m.length > 0) {
        chws.send(m)

        chat.classList.add("hide")
        chat.children[1].value = ""
    }
}

// createChat :: IO ()
function createChat() {
    var wrap = document.createElement("div")
    var chat = document.createElement("input")
    var logs = document.createElement("div")

    chat.addEventListener("keypress", function(e) {
        if (e.keyCode === 13) chatSend(this.value)
    })

    wrap.appendChild(logs)
    wrap.appendChild(chat)
    document.body.appendChild(wrap)

    return wrap
}

// addMsg :: String -> IO ()
function addMsg(m) {
    if (! chat) chat = createChat()

    var msg = document.createElement("span")

    msg.textContent = m

    chat.children[0].appendChild(msg)
}

// toggleChat :: IO ()
function toggleChat(b) {
    if (! chat) chat = createChat()

    if (b) chat.classList.remove("hide")
    else chat.classList.add("hide")

    chat.children[1].focus()
}

// chatter :: Event -> IO ()
function chatter(m) {
    addMsg(m.data)
}

// logger :: Event -> IO ()
function logger(m) {
    log(m.data)
}

// mooder :: Event -> IO ()
function mooder(m) {
    if (m.data === "upset") playerColor = "lightPink"
    else if (m.data === "happy") playerColor = "lightGreen"
    else playerColor = "white"

    draw(player.x, player.y, playerColor)
}

// TODO slow down movement when keyboard is empty, towards even pixel number.
// move :: IO ()
function move() {
    var next = requestAnimationFrame(move)
    moving = true

    // FIXME right/down accelerates left/up to insane speeds when used after
    if (keyElem("right")) {
        stepX = Math.min(++stepX, 10)
        speedX = Math.sin(Math.PI / 2 / playerSize * stepX) * maxSpeed

    } else if (keyElem("left")) {
        stepX = Math.min(++stepX, 10)
        speedX = -Math.sin(Math.PI / 2 / playerSize * stepX) * maxSpeed

    } else {
        stepX = player.x % playerSize
        speedX = Math.cos(Math.PI / 2 / playerSize * stepX) * maxSpeed
    }

    if (keyElem("down")) {
        stepY = Math.min(++stepY, 10)
        speedY = Math.sin(Math.PI / 2 / playerSize * stepY) * maxSpeed

    } else if (keyElem("up")) {
        stepY = Math.min(++stepY, 10)
        speedY = -Math.sin(Math.PI / 2 / playerSize * stepY) * maxSpeed

    } else {
        stepY = player.y % playerSize
        speedY = Math.cos(Math.PI / 2 / playerSize * stepY) * maxSpeed
    }

    player.x += speedX
    player.y += speedY

    // Don't go outside the canvas
    if (player.x < 0) player.x = 0
    else if (player.x >= cv.width - 10) player.x = cv.width - 10
    if (player.y < 0) player.y = 0
    else if (player.y >= cv.height - 10) player.y = cv.height - 10

    log(player.x + " x " + player.y + ": " + speedX + " * " + speedY)

    draw(player.x, player.y, playerColor)

    var emptyKeyboard = keyboard.length === 0
    var evenX = Math.round(player.x) % playerSize === 0
    var evenY = Math.round(player.y) % playerSize === 0

    if (emptyKeyboard && evenX && evenY) {
        speedX = speedY = 0
        cancelAnimationFrame(next)
        moving = false

    }

    if (emptyKeyboard && evenX) speedX = 0
    if (emptyKeyboard && evenY) speedY = 0
}

// draw :: Int -> Int -> IO ()
function draw(x, y, pc) {
    cx.clearRect(0, 0, cv.width, cv.height)
    cx.fillStyle = pc
    cx.fillRect( x
               , y
               , playerSize
               , playerSize
               )
}

// keyElem :: String -> Int -> Bool
function keyElem(k, e) {
    for (var i = 0; i < keyboard.length; i++)
        if (keyConfig[k].indexOf(keyboard[i]) !== -1) return true
}

// XXX do we ever use < 32?
// keydown :: Event -> IO ()
function keydown(e) {
    if (e.target === document.body) {
        if (keyboard.indexOf(e.keyCode) === -1 && e.keyCode >= 32)
            keyboard.push(e.keyCode)

        if (e.keyCode === 13) toggleChat(true)
        else if (e.keyCode === 27) toggleChat(false)

        else if (! moving) requestAnimationFrame(move)

        log(keyboard)
    }
}

// keyup :: Event -> IO ()
function keyup(e) {
    if (e.target === document.body) {
        for (var i = keyboard.length - 1; i >= 0; i--)
            if (keyboard[i] === e.keyCode) keyboard.splice(i, 1)

        if (! moving) requestAnimationFrame(move)

        log(keyboard)
    }
}

// connect :: String -> Int -> String -> String -> IO WebSocket
function connect(host, port, path, protocol, key, f) {
    var ws = new WebSocket("ws://" + host + ':' + port + path)

    ws.onopen = function(_) { ws.send(protocol + " " + key) }
    ws.onclose = function(_) { ws.close() }
    ws.onmessage = f

    return ws
}

// disconnectAll :: IO ()
function disconnectAll() {
    var ss = [mvws, chws, mows]

    for (var i = 0, len = ss.length; i < len; i++) {
        ss[i].onclose = function() {}
        ss[i].onmessage = function() {}
        ss[i].close()
    }

    mvws = undefined
    chws = undefined
    mows = undefined
}

// setupCanvas :: Canvas -> IO Context
function setupCanvas(c) {
    c.width = window.innerWidth
    c.height = window.innerHeight
    var cx = c.getContext("2d")

    cx.fillStyle = playerColor

    return cx
}


function main() {
    key = randomString(10)

    mvws = connect(host, port, path, "move", key, logger)
    chws = connect(host, port, path, "chat", key, chatter)
    mows = connect(host, port, path, "mood", key, mooder)

    window.onbeforeunload = disconnectAll

    cv = document.querySelector("canvas")
    cx = setupCanvas(cv)

    document.body.addEventListener("keydown", keydown)
    document.body.addEventListener("keyup", keyup)

    move(0, 0)
}

