function CodeJar(editor, highlight, opt = {}) {
    const options = Object.assign({ tab: "\t", ownerRoot: window }, opt);
    // only Chrome supports shadowRoot.getSelection
    if (typeof options.ownerRoot.getSelection !== 'function') {
        options.ownerRoot = options.ownerRoot.ownerDocument;
        if (typeof options.ownerRoot.getSelection !== 'function') {
            throw new Error('Impossible to access text selections.');
        }
    }
    let listeners = [];
    let history = [];
    let at = -1;
    let focus = false;
    let callback;
    let prev; // code content prior keydown event
    let isFirefox = navigator.userAgent.toLowerCase().indexOf("firefox") > -1;
    editor.setAttribute("contentEditable", isFirefox ? "true" : "plaintext-only");
    editor.setAttribute("spellcheck", "false");
    editor.style.outline = "none";
    editor.style.overflowWrap = "break-word";
    editor.style.overflowY = "auto";
    editor.style.resize = "vertical";
    editor.style.whiteSpace = "pre-wrap";
    highlight(editor);
    const debounceHighlight = debounce(() => {
        const pos = save();
        highlight(editor);
        restore(pos);
    }, 30);
    let recording = false;
    const shouldRecord = (event) => {
        return !isUndo(event) && !isRedo(event)
            && event.key !== "Meta"
            && event.key !== "Control"
            && event.key !== "Alt"
            && !event.key.startsWith("Arrow");
    };
    const debounceRecordHistory = debounce((event) => {
        if (shouldRecord(event)) {
            recordHistory();
            recording = false;
        }
    }, 300);
    const on = (type, fn) => {
        listeners.push([type, fn]);
        editor.addEventListener(type, fn);
    };
    on("keydown", event => {
        if (event.defaultPrevented)
            return;
        prev = toString();
        handleNewLine(event);
        handleTabCharacters(event);
        handleJumpToBeginningOfLine(event);
        handleSelfClosingCharacters(event);
        handleUndoRedo(event);
        if (shouldRecord(event) && !recording) {
            recordHistory();
            recording = true;
        }
    });
    on("keyup", event => {
        if (event.defaultPrevented)
            return;
        if (event.isComposing)
            return;
        if (prev !== toString())
            debounceHighlight();
        debounceRecordHistory(event);
        if (callback)
            callback(toString());
    });
    on("focus", _event => {
        focus = true;
    });
    on("blur", _event => {
        focus = false;
    });
    on("paste", event => {
        recordHistory();
        handlePaste(event);
        recordHistory();
        if (callback)
            callback(toString());
    });
    function save() {
        const s = options.ownerRoot.getSelection();
        const pos = { start: 0, end: 0, dir: undefined };
        visit(editor, el => {
            if (el === s.anchorNode && el === s.focusNode) {
                pos.start += s.anchorOffset;
                pos.end += s.focusOffset;
                pos.dir = s.anchorOffset <= s.focusOffset ? "->" : "<-";
                return "stop";
            }
            if (el === s.anchorNode) {
                pos.start += s.anchorOffset;
                if (!pos.dir) {
                    pos.dir = "->";
                }
                else {
                    return "stop";
                }
            }
            else if (el === s.focusNode) {
                pos.end += s.focusOffset;
                if (!pos.dir) {
                    pos.dir = "<-";
                }
                else {
                    return "stop";
                }
            }
            if (el.nodeType === Node.TEXT_NODE) {
                if (pos.dir != "->")
                    pos.start += el.nodeValue.length;
                if (pos.dir != "<-")
                    pos.end += el.nodeValue.length;
            }
        });
        return pos;
    }
    function restore(pos) {
        const s = options.ownerRoot.getSelection();
        let startNode, startOffset = 0;
        let endNode, endOffset = 0;
        if (!pos.dir)
            pos.dir = "->";
        if (pos.start < 0)
            pos.start = 0;
        if (pos.end < 0)
            pos.end = 0;
        // Flip start and end if the direction reversed
        if (pos.dir == "<-") {
            const { start, end } = pos;
            pos.start = end;
            pos.end = start;
        }
        let current = 0;
        visit(editor, el => {
            if (el.nodeType !== Node.TEXT_NODE)
                return;
            const len = (el.nodeValue || "").length;
            if (current + len >= pos.start) {
                if (!startNode) {
                    startNode = el;
                    startOffset = pos.start - current;
                }
                if (current + len >= pos.end) {
                    endNode = el;
                    endOffset = pos.end - current;
                    return "stop";
                }
            }
            current += len;
        });
        // If everything deleted place cursor at editor
        if (!startNode)
            startNode = editor;
        if (!endNode)
            endNode = editor;
        // Flip back the selection
        if (pos.dir == "<-") {
            [startNode, startOffset, endNode, endOffset] = [endNode, endOffset, startNode, startOffset];
        }
        s.setBaseAndExtent(startNode, startOffset, endNode, endOffset);
    }
    function beforeCursor() {
        const s = options.ownerRoot.getSelection();
        const r0 = s.getRangeAt(0);
        const r = document.createRange();
        r.selectNodeContents(editor);
        r.setEnd(r0.startContainer, r0.startOffset);
        return r.toString();
    }
    function afterCursor() {
        const s = options.ownerRoot.getSelection();
        const r0 = s.getRangeAt(0);
        const r = document.createRange();
        r.selectNodeContents(editor);
        r.setStart(r0.endContainer, r0.endOffset);
        return r.toString();
    }
    function handleNewLine(event) {
        if (event.key === "Enter") {
            const before = beforeCursor();
            const after = afterCursor();
            let [padding] = findPadding(before);
            let newLinePadding = padding;
            // If last symbol is "{" ident new line
            if (before[before.length - 1] === "{") {
                newLinePadding += options.tab;
            }
            if (isFirefox) {
                preventDefault(event);
                insert("\n" + newLinePadding);
            }
            else {
                // Normal browsers
                if (newLinePadding.length > 0) {
                    preventDefault(event);
                    insert("\n" + newLinePadding);
                }
            }
            // Place adjacent "}" on next line
            if (newLinePadding !== padding && after[0] === "}") {
                const pos = save();
                insert("\n" + padding);
                restore(pos);
            }
        }
    }
    function handleSelfClosingCharacters(event) {
        const open = `([{'"`;
        const close = `)]}'"`;
        const codeAfter = afterCursor();
        if (close.includes(event.key) && codeAfter.substr(0, 1) === event.key) {
            const pos = save();
            preventDefault(event);
            pos.start = ++pos.end;
            restore(pos);
        }
        else if (open.includes(event.key)) {
            const pos = save();
            preventDefault(event);
            const text = event.key + close[open.indexOf(event.key)];
            insert(text);
            pos.start = ++pos.end;
            restore(pos);
        }
    }
    function handleTabCharacters(event) {
        if (event.key === "Tab") {
            preventDefault(event);
            if (event.shiftKey) {
                const before = beforeCursor();
                let [padding, start,] = findPadding(before);
                if (padding.length > 0) {
                    const pos = save();
                    // Remove full length tab or just remaining padding
                    const len = Math.min(options.tab.length, padding.length);
                    restore({ start, end: start + len });
                    document.execCommand("delete");
                    pos.start -= len;
                    pos.end -= len;
                    restore(pos);
                }
            }
            else {
                insert(options.tab);
            }
        }
    }
    function handleJumpToBeginningOfLine(event) {
        if (event.key === "ArrowLeft" && event.metaKey) {
            preventDefault(event);
            const before = beforeCursor();
            let [padding, start, end] = findPadding(before);
            if (before.endsWith(padding)) {
                if (event.shiftKey) {
                    const pos = save();
                    restore({ start, end: pos.end }); // Select from line start.
                }
                else {
                    restore({ start, end: start }); // Jump to line start.
                }
            }
            else {
                if (event.shiftKey) {
                    const pos = save();
                    restore({ start: end, end: pos.end }); // Select from beginning of text.
                }
                else {
                    restore({ start: end, end }); // Jump to beginning of text.
                }
            }
        }
    }
    function handleUndoRedo(event) {
        if (isUndo(event)) {
            preventDefault(event);
            at--;
            const record = history[at];
            if (record) {
                editor.innerHTML = record.html;
                restore(record.pos);
            }
            if (at < 0)
                at = 0;
        }
        if (isRedo(event)) {
            preventDefault(event);
            at++;
            const record = history[at];
            if (record) {
                editor.innerHTML = record.html;
                restore(record.pos);
            }
            if (at >= history.length)
                at--;
        }
    }
    function recordHistory() {
        if (!focus)
            return;
        const html = editor.innerHTML;
        const pos = save();
        const lastRecord = history[at];
        if (lastRecord) {
            if (lastRecord.html === html
                && lastRecord.pos.start === pos.start
                && lastRecord.pos.end === pos.end)
                return;
        }
        at++;
        history[at] = { html, pos };
        history.splice(at + 1);
        const maxHistory = 300;
        if (at > maxHistory) {
            at = maxHistory;
            history.splice(0, 1);
        }
    }
    function handlePaste(event) {
        preventDefault(event);
        const text = (event.originalEvent || event).clipboardData.getData("text/plain");
        const pos = save();
        insert(text);
        highlight(editor);
        restore({ start: pos.end + text.length, end: pos.end + text.length });
    }
    function visit(editor, visitor) {
        const queue = [];
        if (editor.firstChild)
            queue.push(editor.firstChild);
        let el = queue.pop();
        while (el) {
            if (visitor(el) === "stop")
                break;
            if (el.nextSibling)
                queue.push(el.nextSibling);
            if (el.firstChild)
                queue.push(el.firstChild);
            el = queue.pop();
        }
    }
    function isCtrl(event) {
        return event.metaKey || event.ctrlKey;
    }
    function isUndo(event) {
        return isCtrl(event) && !event.shiftKey && event.code === "KeyZ";
    }
    function isRedo(event) {
        return isCtrl(event) && event.shiftKey && event.code === "KeyZ";
    }
    function insert(text) {
        text = text
            .replace(/&/g, "&amp;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;")
            .replace(/"/g, "&quot;")
            .replace(/'/g, "&#039;");
        document.execCommand("insertHTML", false, text);
    }
    function debounce(cb, wait) {
        let timeout = 0;
        return (...args) => {
            clearTimeout(timeout);
            timeout = setTimeout(() => cb(...args), wait);
        };
    }
    function findPadding(text) {
        // Find beginning of previous line.
        let i = text.length - 1;
        while (i >= 0 && text[i] !== "\n")
            i--;
        i++;
        // Find padding of the line.
        let j = i;
        while (j < text.length && /[ \t]/.test(text[j]))
            j++;
        return [text.substring(i, j) || "", i, j];
    }
    function toString() {
        return editor.textContent || "";
    }
    function preventDefault(event) {
        event.preventDefault();
    }
    return {
        updateOptions(options) {
            options = Object.assign(Object.assign({}, options), options);
        },
        updateCode(code) {
            editor.textContent = code;
            highlight(editor);
        },
        onUpdate(cb) {
            callback = cb;
        },
        toString,
        destroy() {
            for (let [type, fn] of listeners) {
                editor.removeEventListener(type, fn);
            }
        },
    };
}

/**
 * Prism: Lightweight, robust, elegant syntax highlighting
 * MIT license http://www.opensource.org/licenses/mit-license.php/
 * @author Lea Verou http://lea.verou.me
 */

// Private helper vars
var lang = /\blang(?:uage)?-([\w-]+)\b/i;
var uniqueId = 0;

var _;
var Prism = _ = {
	codeTag: 'code',

	util: {
		encode: function encode(tokens) {
			if (tokens instanceof Token) {
				return new Token(tokens.type, encode(tokens.content), tokens.alias);
			} else if (Array.isArray(tokens)) {
				return tokens.map(encode);
			} else {
				return tokens.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/\u00a0/g, ' ');
			}
		},

		type: function (o) {
			return Object.prototype.toString.call(o).slice(8, -1);
		},

		objId: function (obj) {
			if (!obj['__id']) {
				Object.defineProperty(obj, '__id', { value: ++uniqueId });
			}
			return obj['__id'];
		},

		// Deep clone a language definition (e.g. to extend it)
		clone: function deepClone(o, visited) {
			var clone, id, type = _.util.type(o);
			visited = visited || {};

			switch (type) {
				case 'Object':
					id = _.util.objId(o);
					if (visited[id]) {
						return visited[id];
					}
					clone = {};
					visited[id] = clone;

					for (var key in o) {
						if (o.hasOwnProperty(key)) {
							clone[key] = deepClone(o[key], visited);
						}
					}

					return clone;

				case 'Array':
					id = _.util.objId(o);
					if (visited[id]) {
						return visited[id];
					}
					clone = [];
					visited[id] = clone;

					o.forEach(function (v, i) {
						clone[i] = deepClone(v, visited);
					});

					return clone;

				default:
					return o;
			}
		},

		/**
		 * Returns the Prism language of the given element set by a `language-xxxx` or `lang-xxxx` class.
		 *
		 * If no language is set for the element or the element is `null` or `undefined`, `none` will be returned.
		 *
		 * @param {Element} element
		 * @returns {string}
		 */
		getLanguage: function (element) {
			while (element && !lang.test(element.className)) {
				element = element.parentElement;
			}
			if (element) {
				return (element.className.match(lang) || [, 'none'])[1].toLowerCase();
			}
			return 'none';
		}
	},

	languages: {
		extend: function (id, redef) {
			var lang = _.util.clone(_.languages[id]);

			for (var key in redef) {
				lang[key] = redef[key];
			}

			return lang;
		},

		/**
		 * Insert a token before another token in a language literal
		 * As this needs to recreate the object (we cannot actually insert before keys in object literals),
		 * we cannot just provide an object, we need an object and a key.
		 * @param inside The key (or language id) of the parent
		 * @param before The key to insert before.
		 * @param insert Object with the key/value pairs to insert
		 * @param root The object that contains `inside`. If equal to Prism.languages, it can be omitted.
		 */
		insertBefore: function (inside, before, insert, root) {
			root = root || _.languages;
			var grammar = root[inside];
			var ret = {};

			for (var token in grammar) {
				if (grammar.hasOwnProperty(token)) {

					if (token == before) {
						for (var newToken in insert) {
							if (insert.hasOwnProperty(newToken)) {
								ret[newToken] = insert[newToken];
							}
						}
					}

					// Do not insert token which also occur in insert. See #1525
					if (!insert.hasOwnProperty(token)) {
						ret[token] = grammar[token];
					}
				}
			}

			var old = root[inside];
			root[inside] = ret;

			// Update references in other language definitions
			_.languages.DFS(_.languages, function(key, value) {
				if (value === old && key != inside) {
					this[key] = ret;
				}
			});

			return ret;
		},

		// Traverse a language definition with Depth First Search
		DFS: function DFS(o, callback, type, visited) {
			visited = visited || {};

			var objId = _.util.objId;

			for (var i in o) {
				if (o.hasOwnProperty(i)) {
					callback.call(o, i, o[i], type || i);

					var property = o[i],
					    propertyType = _.util.type(property);

					if (propertyType === 'Object' && !visited[objId(property)]) {
						visited[objId(property)] = true;
						DFS(property, callback, null, visited);
					}
					else if (propertyType === 'Array' && !visited[objId(property)]) {
						visited[objId(property)] = true;
						DFS(property, callback, i, visited);
					}
				}
			}
		}
	},
	plugins: {},

	highlightElement: function(element, callback) {
		// Find language
		var language = _.util.getLanguage(element);
		var grammar = _.languages[language];

		// Set language on the element, if not present
		element.className = element.className.replace(lang, '').replace(/\s+/g, ' ') + ' language-' + language;

		// Set language on the parent, for styling
		var parent = element.parentNode;
		if (parent && parent.nodeName.toLowerCase() === 'pre') {
			parent.className = parent.className.replace(lang, '').replace(/\s+/g, ' ') + ' language-' + language;
		}

		var code = element.textContent;

		var env = {
			element: element,
			language: language,
			grammar: grammar,
			code: code
		};

		function insertHighlightedCode(highlightedCode) {
			env.highlightedCode = highlightedCode;

			_.hooks.run('before-insert', env);

			env.element.innerHTML = env.highlightedCode;

			_.hooks.run('after-highlight', env);
			_.hooks.run('complete', env);
			callback && callback.call(env.element);
		}

		_.hooks.run('before-sanity-check', env);

		if (!env.code) {
			_.hooks.run('complete', env);
			callback && callback.call(env.element);
			return;
		}

		_.hooks.run('before-highlight', env);

		if (!env.grammar) {
			insertHighlightedCode(_.util.encode(env.code));
			return;
		}

		insertHighlightedCode(_.highlight(env.code, env.grammar, env.language));
	},

	highlight: function (text, grammar, language) {
		var env = {
			code: text,
			grammar: grammar,
			language: language
		};
		_.hooks.run('before-tokenize', env);
		env.tokens = _.tokenize(env.code, env.grammar);
		_.hooks.run('after-tokenize', env);
		return Token.stringify(_.util.encode(env.tokens), env.language);
	},

	tokenize: function(text, grammar) {
		var rest = grammar.rest;
		if (rest) {
			for (var token in rest) {
				grammar[token] = rest[token];
			}

			delete grammar.rest;
		}

		var tokenList = new LinkedList();
		addAfter(tokenList, tokenList.head, text);

		matchGrammar(text, tokenList, grammar, tokenList.head, 0);

		return toArray(tokenList);
	},

	hooks: {
		all: {},

		add: function (name, callback) {
			var hooks = _.hooks.all;

			hooks[name] = hooks[name] || [];

			hooks[name].push(callback);
		},

		run: function (name, env) {
			var callbacks = _.hooks.all[name];

			if (!callbacks || !callbacks.length) {
				return;
			}

			for (var i=0, callback; callback = callbacks[i++];) {
				callback(env);
			}
		}
	},

	Token: Token
};

function Token(type, content, alias, matchedStr, greedy) {
	this.type = type;
	this.content = content;
	this.alias = alias;
	// Copy of the full string this token was created from
	this.length = (matchedStr || '').length|0;
	this.greedy = !!greedy;
}

Token.stringify = function stringify(o, language) {
	if (typeof o == 'string') {
		return o;
	}
	if (Array.isArray(o)) {
		var s = '';
		o.forEach(function (e) {
			s += stringify(e, language);
		});
		return s;
	}

	var env = {
		type: o.type,
		content: stringify(o.content, language),
		tag: 'span',
		classes: ['token', o.type],
		attributes: {},
		language: language
	};

	var aliases = o.alias;
	if (aliases) {
		if (Array.isArray(aliases)) {
			Array.prototype.push.apply(env.classes, aliases);
		} else {
			env.classes.push(aliases);
		}
	}

	_.hooks.run('wrap', env);

	var attributes = '';
	for (var name in env.attributes) {
		attributes += ' ' + name + '="' + (env.attributes[name] || '').replace(/"/g, '&quot;') + '"';
	}

	return '<' + env.tag + ' class="' + env.classes.join(' ') + '"' + attributes + '>' + env.content + '</' + env.tag + '>';
};

/**
 * @param {string} text
 * @param {LinkedList<string | Token>} tokenList
 * @param {any} grammar
 * @param {LinkedListNode<string | Token>} startNode
 * @param {number} startPos
 * @param {boolean} [oneshot=false]
 * @param {string} [target]
 */
function matchGrammar(text, tokenList, grammar, startNode, startPos, oneshot, target) {
	for (var token in grammar) {
		if (!grammar.hasOwnProperty(token) || !grammar[token]) {
			continue;
		}

		var patterns = grammar[token];
		patterns = Array.isArray(patterns) ? patterns : [patterns];

		for (var j = 0; j < patterns.length; ++j) {
			if (target && target == token + ',' + j) {
				return;
			}

			var pattern = patterns[j],
				inside = pattern.inside,
				lookbehind = !!pattern.lookbehind,
				greedy = !!pattern.greedy,
				lookbehindLength = 0,
				alias = pattern.alias;

			if (greedy && !pattern.pattern.global) {
				// Without the global flag, lastIndex won't work
				var flags = pattern.pattern.toString().match(/[imsuy]*$/)[0];
				pattern.pattern = RegExp(pattern.pattern.source, flags + 'g');
			}

			pattern = pattern.pattern || pattern;

			for ( // iterate the token list and keep track of the current token/string position
				var currentNode = startNode.next, pos = startPos;
				currentNode !== tokenList.tail;
				pos += currentNode.value.length, currentNode = currentNode.next
			) {

				var str = currentNode.value;

				if (tokenList.length > text.length) {
					// Something went terribly wrong, ABORT, ABORT!
					return;
				}

				if (str instanceof Token) {
					continue;
				}

				var removeCount = 1; // this is the to parameter of removeBetween

				if (greedy && currentNode != tokenList.tail.prev) {
					pattern.lastIndex = pos;
					var match = pattern.exec(text);
					if (!match) {
						break;
					}

					var from = match.index + (lookbehind && match[1] ? match[1].length : 0);
					var to = match.index + match[0].length;
					var p = pos;

					// find the node that contains the match
					p += currentNode.value.length;
					while (from >= p) {
						currentNode = currentNode.next;
						p += currentNode.value.length;
					}
					// adjust pos (and p)
					p -= currentNode.value.length;
					pos = p;

					// the current node is a Token, then the match starts inside another Token, which is invalid
					if (currentNode.value instanceof Token) {
						continue;
					}

					// find the last node which is affected by this match
					for (
						var k = currentNode;
						k !== tokenList.tail && (p < to || (typeof k.value === 'string' && !k.prev.value.greedy));
						k = k.next
					) {
						removeCount++;
						p += k.value.length;
					}
					removeCount--;

					// replace with the new match
					str = text.slice(pos, p);
					match.index -= pos;
				} else {
					pattern.lastIndex = 0;

					var match = pattern.exec(str);
				}

				if (!match) {
					if (oneshot) {
						break;
					}

					continue;
				}

				if (lookbehind) {
					lookbehindLength = match[1] ? match[1].length : 0;
				}

				var from = match.index + lookbehindLength,
					match = match[0].slice(lookbehindLength),
					to = from + match.length,
					before = str.slice(0, from),
					after = str.slice(to);

				var removeFrom = currentNode.prev;

				if (before) {
					removeFrom = addAfter(tokenList, removeFrom, before);
					pos += before.length;
				}

				removeRange(tokenList, removeFrom, removeCount);

				var wrapped = new Token(token, inside ? _.tokenize(match, inside) : match, alias, match, greedy);
				currentNode = addAfter(tokenList, removeFrom, wrapped);

				if (after) {
					addAfter(tokenList, currentNode, after);
				}


				if (removeCount > 1)
					matchGrammar(text, tokenList, grammar, currentNode.prev, pos, true, token + ',' + j);

				if (oneshot)
					break;
			}
		}
	}
}

/**
 * @typedef LinkedListNode
 * @property {T} value
 * @property {LinkedListNode<T> | null} prev The previous node.
 * @property {LinkedListNode<T> | null} next The next node.
 * @template T
 */

/**
 * @template T
 */
function LinkedList() {
	/** @type {LinkedListNode<T>} */
	var head = { value: null, prev: null, next: null };
	/** @type {LinkedListNode<T>} */
	var tail = { value: null, prev: head, next: null };
	head.next = tail;

	/** @type {LinkedListNode<T>} */
	this.head = head;
	/** @type {LinkedListNode<T>} */
	this.tail = tail;
	this.length = 0;
}

/**
 * Adds a new node with the given value to the list.
 * @param {LinkedList<T>} list
 * @param {LinkedListNode<T>} node
 * @param {T} value
 * @returns {LinkedListNode<T>} The added node.
 * @template T
 */
function addAfter(list, node, value) {
	// assumes that node != list.tail && values.length >= 0
	var next = node.next;

	var newNode = { value: value, prev: node, next: next };
	node.next = newNode;
	next.prev = newNode;
	list.length++;

	return newNode;
}
/**
 * Removes `count` nodes after the given node. The given node will not be removed.
 * @param {LinkedList<T>} list
 * @param {LinkedListNode<T>} node
 * @param {number} count
 * @template T
 */
function removeRange(list, node, count) {
	var next = node.next;
	for (var i = 0; i < count && next !== list.tail; i++) {
		next = next.next;
	}
	node.next = next;
	next.prev = node;
	list.length -= i;
}
/**
 * @param {LinkedList<T>} list
 * @returns {T[]}
 * @template T
 */
function toArray(list) {
	var array = [];
	var node = list.head.next;
	while (node !== list.tail) {
		array.push(node.value);
		node = node.next;
	}
	return array;
}

var lightTheme = "/**\r\n * VS theme by Andrew Lock (https://andrewlock.net)\r\n * Inspired by Visual Studio syntax coloring\r\n */\r\n\r\n code[class*=\"language-\"],\r\n pre[class*=\"language-\"] {\r\n   color: #393A34;\r\n   /* font-family: \"Consolas\", \"Bitstream Vera Sans Mono\", \"Courier New\", Courier, monospace; */\r\n   direction: ltr;\r\n   text-align: left;\r\n   white-space: pre;\r\n   word-spacing: normal;\r\n   word-break: normal;\r\n   /* font-size: .9em; */\r\n   /* line-height: 1.2em; */\r\n \r\n   /* -moz-tab-size: 4; */\r\n   /* -o-tab-size: 4; */\r\n   /* tab-size: 4; */\r\n \r\n   -webkit-hyphens: none;\r\n   -moz-hyphens: none;\r\n   -ms-hyphens: none;\r\n   hyphens: none;\r\n }\r\n \r\n pre > code[class*=\"language-\"] {\r\n   font-size: 1em;\r\n }\r\n \r\n pre[class*=\"language-\"]::-moz-selection, pre[class*=\"language-\"] ::-moz-selection,\r\n code[class*=\"language-\"]::-moz-selection, code[class*=\"language-\"] ::-moz-selection {\r\n   background: #C1DEF1;\r\n }\r\n \r\n pre[class*=\"language-\"]::selection, pre[class*=\"language-\"] ::selection,\r\n code[class*=\"language-\"]::selection, code[class*=\"language-\"] ::selection {\r\n   background: #C1DEF1;\r\n }\r\n \r\n /* Code blocks */\r\n pre[class*=\"language-\"] {\r\n   padding: 1em;\r\n   margin: .5em 0;\r\n   overflow: auto;\r\n   border: 1px solid #dddddd;\r\n   background-color: white;\r\n }\r\n \r\n /* Inline code */\r\n :not(pre) > code[class*=\"language-\"] {\r\n   padding: .2em;\r\n   padding-top: 1px;\r\n   padding-bottom: 1px;\r\n   background: #f8f8f8;\r\n   border: 1px solid #dddddd;\r\n }\r\n \r\n .token.comment,\r\n .token.prolog,\r\n .token.doctype,\r\n .token.cdata {\r\n   color: #008000;\r\n   font-style: italic;\r\n }\r\n \r\n .token.namespace {\r\n   opacity: .7;\r\n }\r\n \r\n .token.string {\r\n   color: #A31515;\r\n }\r\n \r\n .token.punctuation,\r\n .token.operator {\r\n   color: #393A34; /* no highlight */\r\n }\r\n \r\n .token.url,\r\n .token.symbol,\r\n .token.number,\r\n .token.boolean,\r\n .token.variable,\r\n .token.constant,\r\n .token.inserted {\r\n   color: #36acaa;\r\n }\r\n \r\n .token.atrule,\r\n .token.keyword,\r\n .token.attr-value,\r\n .language-autohotkey .token.selector,\r\n .language-json .token.boolean,\r\n .language-json .token.number,\r\n code[class*=\"language-css\"] {\r\n   color: #0000ff;\r\n }\r\n \r\n .token.function {\r\n   color: #393A34;\r\n }\r\n \r\n .token.deleted,\r\n .language-autohotkey .token.tag {\r\n   color: #9a050f;\r\n }\r\n \r\n .token.selector,\r\n .language-autohotkey .token.keyword {\r\n   color: #00009f;\r\n }\r\n \r\n .token.important,\r\n .token.bold {\r\n   font-weight: bold;\r\n }\r\n \r\n .token.italic {\r\n   font-style: italic;\r\n }\r\n \r\n .token.class-name,\r\n .language-json .token.property {\r\n   color: #2B91AF;\r\n }\r\n \r\n .token.tag,\r\n .token.selector {\r\n   color: #800000;\r\n }\r\n \r\n .token.attr-name,\r\n .token.property,\r\n .token.regex,\r\n .token.entity {\r\n   color: #ff0000;\r\n }\r\n \r\n .token.directive.tag .tag {\r\n   background: #ffff00;\r\n   color: #393A34;\r\n }\r\n \r\n /* overrides color-values for the Line Numbers plugin\r\n  * http://prismjs.com/plugins/line-numbers/\r\n  */\r\n\r\n .line-numbers .line-numbers-rows {\r\n   border-right-color: #a5a5a5;\r\n }\r\n \r\n .line-numbers-rows > span:before {\r\n   color: #2B91AF;\r\n }\r\n \r\n /* overrides color-values for the Line Highlight plugin\r\n * http://prismjs.com/plugins/line-highlight/\r\n */\r\n \r\n .line-highlight {\r\n   background: rgba(193, 222, 241, 0.2);\r\n   background: -webkit-linear-gradient(left, rgba(193, 222, 241, 0.2) 70%, rgba(221, 222, 241, 0));\r\n   background: linear-gradient(to right, rgba(193, 222, 241, 0.2) 70%, rgba(221, 222, 241, 0));\r\n }\r\n";

var lineNumbersStyle = "pre[class*=\"language-\"].line-numbers {\r\n\tposition: relative;\r\n\tpadding-left: 3.8em;\r\n\tcounter-reset: linenumber;\r\n}\r\n\r\npre[class*=\"language-\"].line-numbers > div {\r\n\tposition: relative;\r\n\twhite-space: inherit;\r\n}\r\n\r\n.line-numbers .line-numbers-rows {\r\n\tposition: absolute;\r\n\tpointer-events: none;\r\n\ttop: 0;\r\n\tfont-size: 100%;\r\n\tleft: -3.8em;\r\n\twidth: 3em; /* works for line-numbers below 1000 lines */\r\n\tletter-spacing: -1px;\r\n\tborder-right: 1px solid #999;\r\n\r\n\t-webkit-user-select: none;\r\n\t-moz-user-select: none;\r\n\t-ms-user-select: none;\r\n\tuser-select: none;\r\n\r\n}\r\n\r\n\t.line-numbers-rows > span {\r\n\t\tpointer-events: none;\r\n\t\tdisplay: block;\r\n\t\tcounter-increment: linenumber;\r\n\t}\r\n\r\n\t\t.line-numbers-rows > span:before {\r\n\t\t\tcontent: counter(linenumber);\r\n\t\t\tcolor: #999;\r\n\t\t\tdisplay: block;\r\n\t\t\tpadding-right: 0.8em;\r\n\t\t\ttext-align: right;\r\n\t\t}\r\n";

var matchBracesStyle = ".token.punctuation.brace-hover,\r\n.token.punctuation.brace-selected {\r\n\toutline: solid 1px;\r\n}\r\n\r\n.rainbow-braces .token.punctuation.brace-level-1,\r\n.rainbow-braces .token.punctuation.brace-level-5,\r\n.rainbow-braces .token.punctuation.brace-level-9 {\r\n\tcolor: #E50;\r\n\topacity: 1;\r\n}\r\n.rainbow-braces .token.punctuation.brace-level-2,\r\n.rainbow-braces .token.punctuation.brace-level-6,\r\n.rainbow-braces .token.punctuation.brace-level-10 {\r\n\tcolor: #0B3;\r\n\topacity: 1;\r\n}\r\n.rainbow-braces .token.punctuation.brace-level-3,\r\n.rainbow-braces .token.punctuation.brace-level-7,\r\n.rainbow-braces .token.punctuation.brace-level-11 {\r\n\tcolor: #26F;\r\n\topacity: 1;\r\n}\r\n.rainbow-braces .token.punctuation.brace-level-4,\r\n.rainbow-braces .token.punctuation.brace-level-8,\r\n.rainbow-braces .token.punctuation.brace-level-12 {\r\n\tcolor: #E0E;\r\n\topacity: 1;\r\n}\r\n";

var mainStyle = ":host {\r\n  display: block;\r\n  border: 1px black solid;\r\n  border-radius: .5ex;\r\n  box-shadow: 0 2px 2px 0 rgba(0, 0, 0, 0.14), 0 1px 5px 0 rgba(0, 0, 0, 0.12), 0 3px 1px -2px rgba(0, 0, 0, 0.2);\r\n  resize: both;\r\n  overflow: auto;\r\n}\r\n\r\n#source-wrapper {\r\n  width: calc(100% - 2em - 2px);\r\n  height: calc(100% - 2px);\r\n  padding-top: 0;\r\n  padding-bottom: 0;\r\n  white-space: pre-wrap;\r\n}\r\n\r\n#source-wrapper.line-numbers {\r\n  width: calc(100% - 4.8em - 2px);\r\n  margin: 0;\r\n}\r\n\r\n#source {\r\n  font-family: \"SFMono-Regular\", Menlo, Monaco, Consolas, \"Ubuntu Mono\", \"Liberation Mono\", \"Lucida Console\", \"Courier New\", monospace;\r\n  tab-size: 2;\r\n  overflow-y: visible !important;\r\n}\r\n";

/**
 * Plugin name which is used as a class name for <pre> which is activating the plugin
 * @type {String}
 */
var PLUGIN_NAME = 'line-numbers';

/**
 * Regular expression used for determining line breaks
 * @type {RegExp}
 */
var NEW_LINE_EXP = /\n(?!$)/g;

/**
 * Resizes line numbers spans according to height of line of code
 * @param {Element} element <pre> element
 */
var _resizeElement = function (element) {
	var codeStyles = getStyles(element);
	var whiteSpace = codeStyles['white-space'];

	if (whiteSpace === 'pre-wrap' || whiteSpace === 'pre-line') {
		var codeElement = element.querySelector(Prism.codeTag);
		var lineNumbersWrapper = element.querySelector('.line-numbers-rows');
		var lineNumberSizer = element.querySelector('.line-numbers-sizer');
		var codeLines = codeElement.textContent.split(NEW_LINE_EXP);

		if (!lineNumberSizer) {
			lineNumberSizer = document.createElement('span');
			lineNumberSizer.className = 'line-numbers-sizer';

			codeElement.appendChild(lineNumberSizer);
		}

		lineNumberSizer.style.display = 'block';

		codeLines.forEach(function (line, lineNumber) {
			lineNumberSizer.textContent = line || '\n';
			var lineSize = lineNumberSizer.getBoundingClientRect().height;
			lineNumbersWrapper.children[lineNumber].style.height = lineSize + 'px';
		});

		lineNumberSizer.textContent = '';
		lineNumberSizer.style.display = 'none';
	}
};

/**
 * Returns style declarations for the element
 * @param {Element} element
 */
var getStyles = function (element) {
	if (!element) {
		return null;
	}

	return window.getComputedStyle ? getComputedStyle(element) : (element.currentStyle || null);
};

Prism.hooks.add('complete', function (env) {
	if (!env.code) {
		return;
	}

	var code = env.element;
	var pre = code.parentNode;

	// works only for <code> wrapped inside <pre> (not inline)
	if (!pre || !/pre/i.test(pre.nodeName)) {
		return;
	}

	// Abort if line numbers already exists
	if (code.querySelector('.line-numbers-rows')) {
		return;
	}

	var addLineNumbers = false;
	var lineNumbersRegex = /(?:^|\s)line-numbers(?:\s|$)/;

	for (var element = code; element; element = element.parentNode) {
		if (lineNumbersRegex.test(element.className)) {
			addLineNumbers = true;
			break;
		}
	}

	// only add line numbers if <code> or one of its ancestors has the `line-numbers` class
	if (!addLineNumbers) {
		return;
	}

	// Remove the class 'line-numbers' from the <code>
	code.className = code.className.replace(lineNumbersRegex, ' ');
	// Add the class 'line-numbers' to the <pre>
	if (!lineNumbersRegex.test(pre.className)) {
		pre.className += ' line-numbers';
	}

	var match = env.code.match(NEW_LINE_EXP);
	var linesNum = match ? match.length + 1 : 1;
	var lineNumbersWrapper;

	var lines = new Array(linesNum + 1).join('<span></span>');

	lineNumbersWrapper = document.createElement('span');
	lineNumbersWrapper.setAttribute('aria-hidden', 'true');
	lineNumbersWrapper.className = 'line-numbers-rows';
	lineNumbersWrapper.innerHTML = lines;

	if (pre.hasAttribute('data-start')) {
		pre.style.counterReset = 'linenumber ' + (parseInt(pre.getAttribute('data-start'), 10) - 1);
	}

	env.element.appendChild(lineNumbersWrapper);

	_resizeElement(pre);

	Prism.hooks.run('line-numbers', env);
});

Prism.hooks.add('line-numbers', function (env) {
	env.plugins = env.plugins || {};
	env.plugins.lineNumbers = true;
});

/**
 * Global exports
 */
Prism.plugins.lineNumbers = {
	/**
	 * Update line numbers after resizing the text area by mouse
	 * @param {Element} element pre element
	 * @return {undefined}
	 */
	updateLineNumbers: function(element) {
		_resizeElement(element);
	},
	/**
	 * Get node for provided line number
	 * @param {Element} element pre element
	 * @param {Number} number line number
	 * @return {Element|undefined}
	 */
	getLine: function (element, number) {
		if (element.tagName !== 'PRE' || !element.classList.contains(PLUGIN_NAME)) {
			return;
		}

		var lineNumberRows = element.querySelector('.line-numbers-rows');
		var lineNumberStart = parseInt(element.getAttribute('data-start'), 10) || 1;
		var lineNumberEnd = lineNumberStart + (lineNumberRows.children.length - 1);

		if (number < lineNumberStart) {
			number = lineNumberStart;
		}
		if (number > lineNumberEnd) {
			number = lineNumberEnd;
		}

		var lineIndex = number - lineNumberStart;

		return lineNumberRows.children[lineIndex];
	}
};

var MATCH_ALL_CLASS = /(?:^|\s)match-braces(?:\s|$)/;

var BRACE_HOVER_CLASS = /(?:^|\s)brace-hover(?:\s|$)/;
var BRACE_SELECTED_CLASS = /(?:^|\s)brace-selected(?:\s|$)/;

var NO_BRACE_HOVER_CLASS = /(?:^|\s)no-brace-hover(?:\s|$)/;
var NO_BRACE_SELECT_CLASS = /(?:^|\s)no-brace-select(?:\s|$)/;

var PARTNER = {
	'(': ')',
	'[': ']',
	'{': '}',
};

var NAMES = {
	'(': 'brace-round',
	'[': 'brace-square',
	'{': 'brace-curly',
};

var LEVEL_WARP = 12;

var pairIdCounter = 0;

var BRACE_ID_PATTERN = /^(pair-\d+-)(open|close)$/;

/**
 * Returns the brace partner given one brace of a brace pair.
 *
 * @param {HTMLElement} brace
 * @returns {HTMLElement}
 */
function getPartnerBrace(brace) {
	var match = BRACE_ID_PATTERN.exec(brace.id);
	return brace.parentElement.querySelector('#' + match[1] + (match[2] == 'open' ? 'close' : 'open'));
}

/**
 * @this {HTMLElement}
 */
function hoverBrace() {
	for (var parent = this.parentElement; parent; parent = parent.parentElement) {
		if (NO_BRACE_HOVER_CLASS.test(parent.className)) {
			return;
		}
	}

	[this, getPartnerBrace(this)].forEach(function (ele) {
		ele.className = (ele.className.replace(BRACE_HOVER_CLASS, ' ') + ' brace-hover').replace(/\s+/g, ' ');
	});
}
/**
 * @this {HTMLElement}
 */
function leaveBrace() {
	[this, getPartnerBrace(this)].forEach(function (ele) {
		ele.className = ele.className.replace(BRACE_HOVER_CLASS, ' ');
	});
}
/**
 * @this {HTMLElement}
 */
function clickBrace() {
	for (var parent = this.parentElement; parent; parent = parent.parentElement) {
		if (NO_BRACE_SELECT_CLASS.test(parent.className)) {
			return;
		}
	}

	[this, getPartnerBrace(this)].forEach(function (ele) {
		ele.className = (ele.className.replace(BRACE_SELECTED_CLASS, ' ') + ' brace-selected').replace(/\s+/g, ' ');
	});
}

Prism.hooks.add('complete', function (env) {

	/** @type {HTMLElement} */
	var code = env.element;
	var pre = code.parentElement;

	if (!pre || pre.tagName != 'PRE') {
		return;
	}

	// find the braces to match
	/** @type {string[]} */
	var toMatch = [];
	for (var ele = code; ele; ele = ele.parentElement) {
		if (MATCH_ALL_CLASS.test(ele.className)) {
			toMatch.push('(', '[', '{');
			break;
		}
	}

	if (toMatch.length == 0) {
		// nothing to match
		return;
	}

	if (!pre.__listenerAdded) {
		// code blocks might be highlighted more than once
		pre.addEventListener('mousedown', function removeBraceSelected() {
			// the code element might have been replaced
			var code = pre.querySelector(Prism.codeTag);
			Array.prototype.slice.call(code.querySelectorAll('.brace-selected')).forEach(function (element) {
				element.className = element.className.replace(BRACE_SELECTED_CLASS, ' ');
			});
		});
		Object.defineProperty(pre, '__listenerAdded', { value: true });
	}

	/** @type {HTMLSpanElement[]} */
	var punctuation = Array.prototype.slice.call(code.querySelectorAll('span.token.punctuation'));

	/** @type {{ index: number, open: boolean, element: HTMLElement }[]} */
	var allBraces = [];

	toMatch.forEach(function (open) {
		var close = PARTNER[open];
		var name = NAMES[open];

		/** @type {[number, number][]} */
		var pairs = [];
		/** @type {number[]} */
		var openStack = [];

		for (var i = 0; i < punctuation.length; i++) {
			var element = punctuation[i];
			if (element.childElementCount == 0) {
				var text = element.textContent;
				if (text === open) {
					allBraces.push({ index: i, open: true, element: element });
					element.className += ' ' + name;
					element.className += ' brace-open';
					openStack.push(i);
				} else if (text === close) {
					allBraces.push({ index: i, open: false, element: element });
					element.className += ' ' + name;
					element.className += ' brace-close';
					if (openStack.length) {
						pairs.push([i, openStack.pop()]);
					}
				}
			}
		}

		pairs.forEach(function (pair) {
			var pairId = 'pair-' + (pairIdCounter++) + '-';

			var openEle = punctuation[pair[0]];
			var closeEle = punctuation[pair[1]];

			openEle.id = pairId + 'open';
			closeEle.id = pairId + 'close';

			[openEle, closeEle].forEach(function (ele) {
				ele.addEventListener('mouseenter', hoverBrace);
				ele.addEventListener('mouseleave', leaveBrace);
				ele.addEventListener('click', clickBrace);
			});
		});
	});

	var level = 0;
	allBraces.sort(function (a, b) { return a.index - b.index; });
	allBraces.forEach(function (brace) {
		if (brace.open) {
			brace.element.className += ' brace-level-' + (level % LEVEL_WARP + 1);
			level++;
		} else {
			level = Math.max(0, level - 1);
			brace.element.className += ' brace-level-' + (level % LEVEL_WARP + 1);
		}
	});

});

Prism.languages.graphviz = {
  comment: [{
    pattern: /\/\/(?:[^\r\n\\]|\\(?:\r\n?|\n|(?![\r\n])))*|\/\*[\s\S]*?(?:\*\/|$)/,
    greedy: true
  }, {
    pattern: /(^|[^\\:])\/\/.*/,
    lookbehind: !0,
    greedy: !0
  }],
  string: {
    pattern: /(["'])(?:\\(?:\r\n|[\s\S])|(?!\1)[^\\\r\n])*\1/,
    greedy: !0
  },
  'class-name': {
    pattern: /(\b(?:digraph|graph)\s+)\w+/,
    lookbehind: true
  },
  keyword: /\b(?:digraph|graph|node|edge)\b/,
  property: /\b(?:Damping|URL|area|arrowhead|arrowsize|arrowtail|aspect|bb|bgcolor|center|charset|clusterrank|color|colorscheme|comment|compound|concentrate|constraint|decorate|defaultdist|dim|dimen|dir|diredgeconstraints|distortion|dpi|edgeURL|edgehref|edgetarget|edgetooltip|epsilon|esep|fillcolor|fixedsize|fontcolor|fontname|fontnames|fontpath|fontsize|group|headURL|headclip|headhref|headlabel|headport|headtarget|headtooltip|height|href|id|image|imagepath|imagescale|label|labelURL|labelangle|labeldistance|labelfloat|labelfontcolor|labelfontname|labelfontsize|labelhref|labeljust|labelloc|labeltarget|labeltooltip|landscape|layer|layerlistsep|layers|layerselect|layersep|layout|len|levels|levelsgap|lhead|lheight|lp|ltail|lwidth|margin|maxiter|mclimit|mindist|minlen|mode|model|mosek|nodesep|nojustify|normalize|nslimit|nslimit1|ordering|orientation|outputorder|overlap|overlap_scaling|pack|packmode|pad|page|pagedir|pencolor|penwidth|peripheries|pin|pos|quadtree|quantum|rank|rankdir|ranksep|ratio|rects|regular|remincross|repulsiveforce|resolution|root|rotate|rotation|samehead|sametail|samplepoints|scale|searchsize|sep|shape|shapefile|showboxes|sides|size|skew|smoothing|sortv|splines|start|style|stylesheet|tail_lp|tailURL|tailclip|tailhref|taillabel|tailport|tailtarget|tailtooltip|target|tooltip|truecolor|vertices|viewport|voro_margin|weight|width|xlabel)\b/,
  constant: /\b(?:dot|neato|fdp|sfdp|twopi|circo|osage|patchwork|LR|RL|TB|BT|box|polygon|ellipse|oval|circle|point|egg|triangle|plaintext|plain|diamond|trapezium|parallelogram|house|pentagon|hexagon|septagon|octagon|doublecircle|doubleoctagon|tripleoctagon|invtriangle|invtrapezium|invhouse|Mdiamond|Msquare|Mcircle|rect|rectangle|square|star|none|underline|cylinder|note|tab|folder|box3d|component|promoter|cds|terminator|utr|primersite|restrictionsite|fivepoverhang|threepoverhang|noverhang|assembly|signature|insulator|ribosite|rnastab|proteasesite|proteinstab|rpromoter|rarrow|larrow|lpromoter|solid|dashed|dotted|bold|rounded|diagonals|filled|striped|wedged)\b/,
  boolean: /\b(?:true|yes|false|no)\b/,
  number: /\b0x[\da-f]+\b|(?:\b\d+\.?\d*|\B\.\d+)(?:e[+-]?\d+)?/i,
  operator: /->|--|=/,
  punctuation: /[{}[\];(),.:]/
};

const features = ['line-numbers', 'match-braces', 'rainbow-braces'];
const connectedKey = Symbol('connecte');
const valueKey = Symbol('value');
const tabKey = Symbol('tab');
const classKey = Symbol('class');
const jarKey = Symbol('jar');
const widthKey = Symbol('width');
const heightKey = Symbol('height');
const mousedownKey = Symbol('mousedown');
const mouseupKey = Symbol('mouseup');
let template, pendingAllLineNumbersUpdate;

Prism.codeTag = 'div';

function getTemplate () {
  if (!template) {
    template = document.createElement('template');
    template.innerHTML = `<style>
${lightTheme}
${lineNumbersStyle}
${matchBracesStyle}
${mainStyle}
</style>
<pre id=source-wrapper><div id=source></div></pre>`;
  }
  return template
}

function triggerUpdateEvent (element, type, detail) {
  element.dispatchEvent(new CustomEvent('update', { bubbles: true, detail }));
}

function getFeatureClasses (element) {
  return element.className
    .trim()
    .split(/\s+/)
    .filter(feature => features.includes(feature))
    .join(' ')
}

function hasChangedFeatureClass (oldValue, newValue) {
  /* c8 ignore next */
  const oldClasses = oldValue ? oldValue.trim().split(/\s+/) : [];
  const newClasses = newValue.trim().split(/\s+/);
  return features.some(feature =>
    oldClasses.includes(feature) !== newClasses.includes(feature))
}

function normalizeLineBreaks (value) {
  return value.replace(/\r?\n/, '\n')
}

function createEditor (element) {
  const ownerRoot = element.shadowRoot;
  const source = ownerRoot.getElementById('source');
  const featureClasses = getFeatureClasses(element);
  source.className = `language-graphviz ${featureClasses}`;
  const jar = element[jarKey] = CodeJar(source, Prism.highlightElement, { tab: element.tab, ownerRoot });
  updateEditor(element);
  jar.onUpdate(newCode => {
    newCode = normalizeLineBreaks(newCode);
    if (element.value !== newCode) {
      element[valueKey] = newCode;
      triggerUpdateEvent(element);
    }
  });
}

function destroyEditor (element) {
  element[jarKey].destroy();
  element[jarKey] = null;
}

function rebuildEditor (element) {
  destroyEditor(element);
  createEditor(element);
}

function updateEditor (element) {
  element[jarKey].updateCode(element.value);
}

function configureEditor (element) {
  element[jarKey].updateOptions({ tab: element.tab });
}

class GraphvizScriptEditorElement extends HTMLElement {
  constructor () {
    super();
    this[valueKey] = '';
    this[tabKey] = '  ';
    this[classKey] = '';
    this.attachShadow({ mode: 'open' });
    this.shadowRoot.appendChild(getTemplate().content.cloneNode(true));
  }

  get value () { return this[valueKey] }
  set value (value) { this.setAttribute('value', value); }

  get tab () { return this[tabKey] }
  set tab (value) { this.setAttribute('tab', value); }

  get className () { return this[classKey] }
  set className (value) { this.setAttribute('class', value); }

  attributeChangedCallback (name, oldValue, newValue) {
    switch (name) {
      case 'value':
        this[valueKey] = normalizeLineBreaks(newValue);
        if (this[connectedKey]) updateEditor(this);
        break
      case 'tab':
        this[tabKey] = newValue;
        if (this[connectedKey]) configureEditor(this);
        break
      case 'class':
        this[classKey] = newValue;
        if (this[connectedKey] && hasChangedFeatureClass(oldValue, newValue)) rebuildEditor(this);
        break
    }
  }

  connectedCallback () {
    registerEvents(this);
    createEditor(this);
    this[connectedKey] = true;
  }

  disconnectedCallback () {
    destroyEditor(this);
    unregisterEvents(this);
    this[connectedKey] = false;
  }

  static get observedAttributes () { return ['value', 'tab', 'class'] }
}

function rememberSize (element) {
  element[widthKey] = element.clientWidth;
  element[heightKey] = element.clientHeight;
}

function updateLineNumbers (element) {
  const newWidth = element.clientWidth;
  const newHeight = element.clientHeight;
  if (newWidth !== element[widthKey] || newHeight !== element[heightKey]) {
    Prism.plugins.lineNumbers.updateLineNumbers(element.shadowRoot.getElementById('source-wrapper'));
    element[widthKey] = newWidth;
    element[heightKey] = newHeight;
  }
}

function registerEvents (element) {
  const mousedown = element[mousedownKey] = () => rememberSize(element);
  const mouseup = element[mouseupKey] = () => updateLineNumbers(element);
  element.addEventListener('mousedown', mousedown);
  element.addEventListener('mouseup', mouseup);
}

function unregisterEvents (element) {
  element.removeEventListener('mouseup', element[mouseupKey]);
  element.removeEventListener('mousedown', element[mousedownKey]);
}

function updateAllLineNumbers () {
  clearTimeout(pendingAllLineNumbersUpdate);
  const editors = document.querySelectorAll('graphviz-script-editor.line-numbers');
  for (const editor of editors) updateLineNumbers(editor);
}

function scheduleUpdateAllLineNumbers () {
  if (pendingAllLineNumbersUpdate) clearTimeout(pendingAllLineNumbersUpdate);
  pendingAllLineNumbersUpdate = setTimeout(updateAllLineNumbers, 300);
}

addEventListener('resize', scheduleUpdateAllLineNumbers);

customElements.define('graphviz-script-editor', GraphvizScriptEditorElement);

export { GraphvizScriptEditorElement as default };
//# sourceMappingURL=script-editor.min.mjs.map
