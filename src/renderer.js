import { Graphviz } from '@hpcc-js/wasm/graphviz'

let graphviz
let fatalError

// if (!self.document) self.document = {} // workaround for a bug in @hpcc-js/wasm

async function receiveRequest ({ data }) {
  const { script } = data
  if (script === undefined) return // prevent endless message loop in tests
  if (fatalError) return postMessage({ error: fatalError })
  try {
    if (!graphviz) graphviz = await Graphviz.load()
    const svg = graphviz.dot(script)
    postMessage({ svg })
  } catch ({ message }) {
    postMessage({ error: { message } })
  }
}

/* istanbul ignore next */ // covered, but reloading the page resets the log
function handleRejection (event) {
  event.preventDefault()
  const { message } = event.reason
  const error = { message: `Graphviz failed. ${message}` }
  if (message.includes('fetching of the wasm failed')) fatalError = error
  postMessage({ error })
}

addEventListener('message', receiveRequest)
addEventListener('unhandledrejection', handleRejection)
