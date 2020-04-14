const express = require('express')
const path = require('path')
const PORT = process.env.PORT || 5000


express()
  .use(express.static(__dirname))
  .get('/', (req, res) => res.sendFile('index.html'))
  .listen(PORT, () => console.log(`Listening on ${ PORT }`))
