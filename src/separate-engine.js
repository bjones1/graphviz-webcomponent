const { delayWorkerLoading } = window.graphvizWebComponent || {}
let renderer, rendererUrl

if (!delayWorkerLoading) setTimeout(getRenderer)

function ensureConfiguration () {
  if (!rendererUrl) {
    ({
      rendererUrl = 'https://unpkg.com/graphviz-webcomponent@1.1.0/dist/renderer.min.js'
    } = window.graphvizWebComponent || {})
  }
}

export default function getRenderer () {
  if (!renderer) {
    ensureConfiguration()
    renderer = new Worker(rendererUrl)
  }
  return renderer
}
