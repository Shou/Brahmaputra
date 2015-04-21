
// Author: Benedict Aas
// Copyright: Benedict Aas
// License: MIT
// License file: LICENSE

var d = document
var qs = Document.prototype.querySelector.bind(d)

// {{{ Constants

// host, path :: String
var host = window.location.host
var path = "/"
// port :: Int
var port = 8080

// playerSize, maxSpeed :: Int
var playerSize = 10
// | Max speed per second
var maxSpeed = 50

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

// | Old timestamp for rAF(movePlayers)
// ots :: Double
var ots = 0

// | Timestamp of rAF(movePlayers) first run
// fts :: Double
var fts = 0

// | Player movement vector websocket
// movews :: WebSocket
var movews
// | Chat websocket
// chatws :: WebSocket
var chatws
// | Mood websocket
// moodws :: WebSocket
var moodws

// | Delay before reconnecting to Websocket
// connectDelay :: Int
var connectDelay = 500

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

// {{{ Functional programming

// Curry helper function
var _cur = function(f) {
    var args = [].slice.call(arguments, 1)

    return function() {
        return f.apply(this, args.concat([].slice.call(arguments, 0)))
    }
}

// | Currying, the best thing to pour over a dish of chicken and rice.
// cu :: (a -> b -> n) -> (a -> (b -> n))
var cu = function(f, len) {
    var args = [].slice.call(arguments, 1)
      , len = len || f.length

    return function() {
        if (arguments.length < len) {
            var comb = [f].concat([].slice.call(arguments, 0))

            return len - arguments.length > 0
                ? cu(_cur.apply(this, comb), len - arguments.length)
                : _cur.call(this, comb)

        } else
            return f.apply(this, arguments)
    }
}

// id :: a -> a
function id(a) { return a }

// | Equivalent to Haskell's void
// abyss :: a -> IO ()
function abyss(_) {}

// _elem :: a -> [a] -> Bool
function _elem(x, xs) {
    for (var i = 0, l = xs.length; i < l; i++) if (x === xs[i]) return true
    return false
}

// keys :: Object k e -> [k]
function _keys(o) {
    var _ = []
    for (k in o) _.push(k)
    return _
}

var keys = cu(_keys)

// elems :: Object k e -> [e]
function _elems(o) {
    var _ = []
    for (k in o) _.push(o[k])
    return _
}

var elems = cu(_elems)

// | Curried elem function
// elem :: a -> [a] -> Bool
var elem = cu(_elem)

// _concat :: [[a]] -> [a]
function _concat(xs) {
    var tmp = []
    for (var i = 0, l = xs.length; i < l; i++)
        for (var j = 0, l = xs[i].length; i < l; i++)
            tmp.push(xs[i][j])

    return tmp
}

// | Curried concat function
var concat = cu(_concat)

// | Accumulate a result `z' from applying `f' to `z' and `xs[i]'.
// foldr :: (a -> b -> b) -> b -> [a] -> b
function _foldr(f, z, xs) {
    for (var i = 0; i < xs.length; i++) z = f(z, xs[i])
    return z
}

var foldr = cu(_foldr)

// | foldr treating `z' as the first element in `xs' instead.
function _foldr1(f, xs){
    if (xs.length > 0) return foldr(f, head(xs), tail(xs))
    else console.error("foldr1: empty list")
}

var foldr1 = cu(_foldr1)

// | Function composition, basically f(g(h(x))) = co(f, g, h)(x)
//   It is easier to read for human beans, and re-created with inspiration from
//   Haskell.
function co() {
    return foldr1(function(f, g) {
        return function(x) { return f(g(x)) }
    }, arguments)
}

// }}}

// chatSend :: String -> IO ()
function chatSend(m) {
    if (m.length > 0) {
        chatws.send(m)

        toggleChat(false)
        chat.children[1].value = ""

    }
}

// createChat :: IO ()
function createChat() {
    var wrap = d.createElement("div")
    var chat = d.createElement("input")
    var logs = d.createElement("div")

    chat.addEventListener("keypress", function(e) {
        if (e.keyCode === 13) chatSend(this.value)
    })

    wrap.appendChild(logs)
    wrap.appendChild(chat)
    d.body.appendChild(wrap)

    return wrap
}

// talkToMeBaby :: String -> IO ()
function talkToMeBaby(text) {
    var aud = frames[0].document.querySelector("audio")

    if (! aud) aud = frames[0].document.body.appendChild(audio)

    aud.src = voiceURL + encodeURIComponent(text)
    aud.play()
}

// addMsg :: String -> IO ()
function addMsg(m) {
    if (! chat) chat = createChat()

    var msg = d.createElement("span")

    var mgps = m.split(' ')

    var mtyp = mgps[0]
    var mnam = mgps[1]
    var mdat = new Date(parseInt(mgps[2]) * 1000)
    var mmsg = mgps.slice(3).join(' ')

    msg.textContent = mnam + ": " + mmsg + ""
    msg.title = mdat.toLocaleString()

    if (mtyp == 'c') talkToMeBaby(mmsg)

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
            players[key].x = parseInt(args[2]) * maxSpeed * 0.001
            players[key].y = parseInt(args[3]) * maxSpeed * 0.001
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

// movePlayers :: Double -> IO ()
function movePlayers(ts) {
    var next = requestAnimationFrame(movePlayers)
    moving = false

    // Clear the canvas
    cx.clearRect(0, 0, cv.width, cv.height)

    ts = ts * 0.001
    if (ots === null) ots = ts - 0.01
    var speed = (ts - ots) * maxSpeed

    for (key in players) moving = movePlayer(key, speed) || moving

    ots = ts

    if (! moving) {
        ots = null
        cancelAnimationFrame(next)
    }
}

// movePlayer :: String -> IO Bool
function movePlayer(key, speed) {
    var ox = players[key].x
    var oy = players[key].y

    players[key].x += players[key].speedX * speed
    players[key].y += players[key].speedY * speed

    // TODO merge this with directly above
    if (players[key].x < 0) players[key].x = 0
    if (players[key].y < 0) players[key].y = 0

    drawPlayer(key)

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
function keyMove(s, n) {
    for (i = 0, l = keyboard.length; i < l; i++) {
        // XXX hack: override n with arg n, to ensure unary coords change
        if (s !== 0) n = keyboard[i]

        if (elem(n, keyConfig.right)) players[selfKey].speedX = s
        else if (elem(n, keyConfig.left)) players[selfKey].speedX = -s
        else if (elem(n, keyConfig.down)) players[selfKey].speedY = s
        else if (elem(n, keyConfig.up)) players[selfKey].speedY = -s
    }

    log(players[selfKey].speedX + " x " + players[selfKey].speedY)
}

// keydown :: Event -> IO ()
function keydown(e) {
    if (e.target === d.body && keyboard.indexOf(e.keyCode) === -1) {
        if (e.keyCode === 13) toggleChat(true)

        else {
            keyboard.push(e.keyCode)

            keyMove(1)

            movews.send(players[selfKey].speedX + " " + players[selfKey].speedY)

            if (! moving) {
                requestAnimationFrame(movePlayers)
            }
        }

    } else if (e.keyCode === 27) toggleChat(false)
}

// keyup :: Event -> IO ()
function keyup(e) {
    if (e.target === d.body && keyboard.indexOf(e.keyCode) !== -1) {
        keyMove(0, e.keyCode)

        keyboard.splice(keyboard.indexOf(e.keyCode), 1)

        var isMovementKey = elem(e.keyCode, concat(elems(keyConfig)))
        if (isMovementKey)
            movews.send(players[selfKey].speedX + " " + players[selfKey].speedY)
    }
}

// connect :: String -> Int -> String -> String -> IO WebSocket
function connect(host, port, path, pcol, key, f) {
    var ws = new WebSocket("//" + host + ':' + port + path)

    ws.onopen = function(_) { ws.send(pcol + " " + key) }
    ws.onclose = function(_) {
        log(_)

        connectDelay = Math.min(connectDelay * 2, 64000)
        setTimeout(function() {
            log("Reconnecting to " + pcol)
            connect(host, port, path, pcol, key, f)
        }, connectDelay)
    }
    ws.onmessage = f

    window[pcol + "ws"] = ws

    log("Connected to " + pcol)
}

// disconnectAll :: IO ()
function disconnectAll() {
    var ss = [movews, chatws, moodws]

    for (var i = 0, len = ss.length; i < len; i++) {
        ss[i].onclose = function() {}
        ss[i].onmessage = function() {}
        ss[i].close()
    }

    movews = undefined
    chatws = undefined
    moodws = undefined
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

    connect(host, port, path, "move", selfKey, mover)
    connect(host, port, path, "chat", selfKey, chatter)
    connect(host, port, path, "mood", selfKey, mooder)

    window.onbeforeunload = disconnectAll

    audio = qs("audio")
    iframe = qs("iframe")

    cv = qs("canvas")
    cx = setupCanvas(cv)

    d.body.addEventListener("keydown", keydown)
    d.body.addEventListener("keyup", keyup)

    drawPlayer(selfKey)
    requestAnimationFrame(movePlayers)
    requestAnimationFrame(function() {
        fts = (new Date()).getTime() * 0.001
    })
}

