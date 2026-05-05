/* The browsers interpretation of the CORS origin policy prevents to run
   webworkers from javascript files fetched from the file:// protocol. This hack
   is to workaround this restriction. */
function createWebWorker() {
  var searchs = search_urls.map((search_url) => {
    let parts = document.location.href.split("/");
    parts[parts.length - 1] = search_url;
    return '"' + parts.join("/") + '"';
  });
  blobContents = ["importScripts(" + searchs.join(",") + ");"];
  var blob = new Blob(blobContents, { type: "application/javascript" });
  var blobUrl = URL.createObjectURL(blob);

  var worker = new Worker(blobUrl);
  URL.revokeObjectURL(blobUrl);

  return worker;
}

var worker;
var waiting = 0;

function wait() {
  waiting = waiting + 1;
  document.querySelector(".search-snake").classList.add("search-busy");
}

function stop_waiting() {
  if (waiting > 0) waiting = waiting - 1;
  else waiting = 0;
  if (waiting == 0) {
    document.querySelector(".search-snake").classList.remove("search-busy");
  }
}

document.querySelector(".search-bar").addEventListener("focus", (ev) => {
  if (typeof worker == "undefined") {
    worker = createWebWorker();
    worker.onmessage = (e) => {
      stop_waiting();
      let results = e.data;
      let search_results = document.querySelector(".search-result");
      search_results.innerHTML = "";
      let f = (entry) => {
        let search_result = document.createElement("a");
        search_result.classList.add("search-entry");
        search_result.href = base_url + entry.url;
        search_result.innerHTML = entry.html;
        search_results.appendChild(search_result);
      };
      results.forEach(f);
      let search_request = document.querySelector(".search-bar").value;
      if (results.length == 0 && search_request != "") {
        let no_result = document.createElement("div");
        no_result.classList.add("search-no-result");
        no_result.innerText = "No result...";
        search_results.appendChild(no_result);
      }
    };
  }
});

document.querySelector(".search-bar").addEventListener("input", (ev) => {
  wait();
  worker.postMessage(ev.target.value);
});


/** Navigation */

let search_result_elt = document.querySelector(".search-result")

function search_results() {
  return search_result_elt.children;
}

function enter_search() {
  document.querySelector(".search-bar").focus();
}

function escape_search() {
  document.activeElement.blur()
}

function focus_previous_result() {
  let results = Array.from(search_results());
  let current_focus = results.findIndex((elt) => (document.activeElement === elt));
  if (current_focus === -1)
    return;
  else if (current_focus === 0)
    enter_search();
  else
    results[current_focus - 1].focus();
}

function focus_next_result() {
  let results = Array.from(search_results());
  if (results.length === 0) return;
  let current_focus = results.findIndex((elt) => (document.activeElement === elt));
  if (current_focus === -1)
    results[0].focus();
  else if (current_focus + 1 === results.length)
    return;
  else
    results[current_focus + 1].focus();
}


function is_searching() {
  return (document.querySelectorAll(".odoc-search:focus-within").length > 0);
}

function is_typing() {
  return (document.activeElement === document.querySelector(".search-bar"));
}

function handle_key_down(event) {
  if (is_searching()) {
    if (event.key === "ArrowUp") {
      event.preventDefault();
      focus_previous_result();
    }
    if (event.key === "ArrowDown") {
      event.preventDefault();
      focus_next_result();
    }
    if (event.key === "Escape") {
      event.preventDefault();
      escape_search();
    }
  }
  if (!(is_typing())) {
    let ascii = event.key.charCodeAt(0);
    if (event.key === "/") {
      event.preventDefault();
      enter_search();
    }
    else if (  is_searching()
            && event.key.length === 1
            && (  (ascii >= 65 && ascii <= 90) // lowercase letter
               || (ascii >= 97 && ascii <= 122) // uppercase letter
               || (ascii >= 48 && ascii <= 57) // numbers
               || (ascii >= 32 && ascii <= 46) // ` ` to `.`
               || (ascii >= 58 && ascii <= 64)) // `:` to `@`
            )
      // We do not prevent default because we want the char to be added to the input
      enter_search ();
  }
}
document.addEventListener("keydown", handle_key_down);
