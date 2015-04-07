
// {{{ Constants

// host, path :: String
var host = window.location.host
var path = "/"
// port :: Int
var port = 8080

// playerSize, maxSpeed :: Int
var playerSize = 10
var maxSpeed = 2
// speedStep :: Float
var speedStep = maxSpeed / 10

// selfKey :: String
var selfKey


// voiceURL :: String
var voiceURL = "https://translate.google.com/translate_tts?ie=UTF-8&tl=ja&q="

// audio :: AudioElement
var audio

// iframe :: IFrameElement
var iframe

// }}}

// {{{ Data

// Player :: Int -> Int -> Int -> Int -> String -> Player
function Player(x, y, sX, sY, color) {
    this.x = x
    this.y = y
    this.speedX = sX
    this.speedY = sY
    this.color = color
}

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

// players :: Object String Player
var players = {}

// moving :: Bool
var moving = false

// | Player movement vector websocket
// mvws :: WebSocket
var mvws
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

// chat :: Element
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

        toggleChat(false)
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

// talkToMeBaby :: String -> IO ()
function talkToMeBaby(text) {
    var aud = top.frames[0].document.body.querySelector("audio")

    if (! aud) aud = top.frames[0].document.body.appendChild(audio)

    aud.src = voiceURL + encodeURIComponent(text)
    aud.play()
}

// addMsg :: String -> IO ()
function addMsg(m) {
    if (! chat) chat = createChat()

    var msg = document.createElement("span")

    msg.textContent = m

    var mcon = m.split(' ').slice(2).join(' ').slice(0, 100)
    talkToMeBaby(mcon)

    chat.children[0].appendChild(msg)
}

// toggleChat :: IO ()
function toggleChat(b) {
    log("toggleChat(" + b + ")")
    if (! chat) chat = createChat()

    if (b) {
        chat.classList.remove("hide")
        chat.children[1].focus()
    } else chat.classList.add("hide")
}

// chatter :: Event -> IO ()
function chatter(m) {
    addMsg(m.data)
}

// mover :: Event -> IO ()
function mover(m) {
    log(m.data)
    var args = m.data.split(' ')

    var key = args[0]
    if (! (key === selfKey)) {
        if (! (key in players)) {
            log(key + " adding to players")
            players[key] = new Player(0, 0, 0, 0, "white")
        }

        var pcol = args[1]

        // Speed vector
        if (pcol === "v") {
            players[key].speedX = parseInt(args[2])
            players[key].speedY = parseInt(args[3])

        // Coordinates
        } else if (pcol === "c") {
            players[key].x = parseInt(args[2])
            players[key].y = parseInt(args[3])
        }

        if (! moving) requestAnimationFrame(movePlayers)
    }
}

// mooder :: Event -> IO ()
function mooder(m) {
    var md = m.data.split(' ')
    playerMood(md[0], md[1])
}

// playerMood :: String -> String -> IO ()
function playerMood(key, mood) {
    if (mood === "upset") players[key].color = "lightPink"
    else if (mood === "happy") players[key].color = "lightGreen"
    else players[key].color = "white"

    drawPlayer(key)
}

// movePlayers :: IO ()
function movePlayers() {
    var next = requestAnimationFrame(movePlayers)
    moving = false

    // Clear the canvas
    cx.clearRect(0, 0, cv.width, cv.height)

    for (key in players) moving = moving || movePlayer(key)

    if (! moving) cancelAnimationFrame(next)
}

// movePlayer :: String -> IO Bool
function movePlayer(key) {
    var ox = players[key].x
    var oy = players[key].y

    players[key].x += players[key].speedX * 5
    players[key].y += players[key].speedY * 5

    drawPlayer(key)

    if (ox !== players[key].x || oy !== players[key].y) log(key + " moving")
    return ! (ox === players[key].x && oy === players[key].y)
}

// TODO slow down movement when keyboard is empty, towards even pixel number.
// move :: IO ()
function move() {
    var next = requestAnimationFrame(move)
    moving = true

    // FIXME right/down accelerates left/up to insane speeds when used in quick
    //       succession
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

    player.x += player.speedX
    player.y += player.speedY

    // Don't go outside the canvas
    if (player.x < 0) player.x = 0
    else if (player.x >= cv.width - 10) player.x = cv.width - 10
    if (player.y < 0) player.y = 0
    else if (player.y >= cv.height - 10) player.y = cv.height - 10

    log(player.x + " x " + player.y + ": " + speedX + " * " + speedY)

    drawPlayer(selfKey)

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

// draw :: String -> IO ()
function drawPlayer(key) {
    cx.fillStyle = players[key].color
    cx.fillRect( players[key].x
               , players[key].y
               , playerSize
               , playerSize
               )
}

// keyMove :: Int -> IO ()
function keyMove(s) {
    for (i = 0, l = keyboard.length; i < l; i++) {
        var n = keyboard[i]

        for (var j = 0, l = keyConfig.up.length; j < l; j++)
            if (n === keyConfig.up[j]) {
                players[selfKey].speedY = -s
            }
        for (var j = 0, l = keyConfig.down.length; j < l; j++)
            if (n === keyConfig.down[j]) {
                players[selfKey].speedY = s
            }
        for (var j = 0, l = keyConfig.right.length; j < l; j++)
            if (n === keyConfig.right[j]) {
                players[selfKey].speedX = s
            }
        for (var j = 0, l = keyConfig.left.length; j < l; j++)
            if (n === keyConfig.left[j]) {
                players[selfKey].speedX = -s
            }
    }

    log(players[selfKey].speedX + " x " + players[selfKey].speedY)
}

// keydown :: Event -> IO ()
function keydown(e) {
    if (e.target === document.body && keyboard.indexOf(e.keyCode) === -1) {
        if (e.keyCode === 13) toggleChat(true)

        else {
            keyboard.push(e.keyCode)

            keyMove(1)

            mvws.send(players[selfKey].speedX + " " + players[selfKey].speedY)

            if (! moving) requestAnimationFrame(movePlayers)
        }

    } else if (e.keyCode === 27) toggleChat(false)
}

// keyup :: Event -> IO ()
function keyup(e) {
    if (e.target === document.body && keyboard.indexOf(e.keyCode) !== -1) {
        keyMove(0)

        keyboard.splice(keyboard.indexOf(e.keyCode), 1)

        mvws.send(players[selfKey].speedX + " " + players[selfKey].speedY)
    }
}

// connect :: String -> Int -> String -> String -> IO WebSocket
function connect(host, port, path, protocol, key, f) {
    var ws = new WebSocket("ws://" + host + ':' + port + path)

    ws.onopen = function(_) { ws.send(protocol + " " + key) }
    ws.onclose = function(_) {  }
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

    return cx
}


function main() {
    selfKey = randomString(10)

    players[selfKey] = new Player(0, 0, 0, 0, "white")

    mvws = connect(host, port, path, "move", selfKey, mover)
    chws = connect(host, port, path, "chat", selfKey, chatter)
    mows = connect(host, port, path, "mood", selfKey, mooder)

    window.onbeforeunload = disconnectAll

    audio = document.querySelector("audio")
    iframe = document.querySelector("iframe")

    cv = document.querySelector("canvas")
    cx = setupCanvas(cv)

    document.body.addEventListener("keydown", keydown)
    document.body.addEventListener("keyup", keyup)

    drawPlayer(selfKey)
    movePlayers()
}

