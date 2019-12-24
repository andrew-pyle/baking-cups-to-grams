// See https://developers.google.com/web/fundamentals/primers/service-workers/
const APP_VERSION = "0.3.2";
const CACHE_NAME = `baking-tools-v${APP_VERSION}`;
const GOOGLE_FONTS_DOMAINS_REGEX = /fonts.(googleapis|gstatic).com/;

/* ACTIVATE */

self.addEventListener("activate", event => {
  event.waitUntil(
    caches.keys().then(cacheNames => {
      // Make a list of old caches to delete from the browser
      const cacheNamesToDelete = cacheNames.filter(
        cacheName => cacheName !== CACHE_NAME
      );
      // Transform list of old caches into array of promises for deleting them
      const cachesBeingDeleted = cacheNamesToDelete.map(cacheName =>
        caches.delete(cacheName)
      );
      // Only let `waitUntil()` resolve after all the caches have been deleted
      return Promise.all(cachesBeingDeleted);
    })
  );
  console.log(serviceWorkerLog("Activated. Old caches have been cleared"));
});

/* INSTALL */

const urlsToCacheOnInitialPageLoad = [
  "/",
  "/index.html",
  "/elm-dist.js",
  "/styles.css",
  "/site.webmanifest"
];

// Install
self.addEventListener("install", event => {
  event.waitUntil(
    caches.open(CACHE_NAME).then(cache => {
      return cache.addAll(urlsToCacheOnInitialPageLoad);
    })
  );
  console.log(
    serviceWorkerLog(`Finished installing. Cache name: ${CACHE_NAME}`)
  );
});

/* INTERCEPT FETCHES */

// Original Based on https://developers.google.com/web/fundamentals/primers/service-workers/
self.addEventListener("fetch", async event => {
  event.respondWith(
    caches.match(event.request).then(response => {
      // Cache hit
      if (response) {
        return response;
      }
      // Cache miss
      else {
        return fetch(event.request).then(response => {
          // Cache good responses
          if (
            // Only keep first-party 'ok' responses
            (response &&
              response.status === 200 &&
              response.type === "basic") ||
            // Also keep opaque responses from Google Fonts
            GOOGLE_FONTS_DOMAINS_REGEX.test(event.request.url)
          ) {
            // Cache a copy of the response
            const responseClone = response.clone();
            caches
              .open(CACHE_NAME)
              .then(cache => cache.put(event.request, responseClone));

            // Also send response on to the browser
            return response;
          }
          // Don't cache junk responses, but do send them on to the browser
          else {
            return response;
          }
        });
      }
    })
  );
});

function serviceWorkerLog(message, version = APP_VERSION) {
  return `[Service Worker ${version}] ${message}`;
}
