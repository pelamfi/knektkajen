# Project Knektkajen

## Project status

So far there is nothing here. Just trying out various web technologies
and getting the feel for them.

## Run Project

```sh
npm install
npm start
# in another tab
npm run webpack
```

After you see the webpack compilation succeed (the `npm run webpack` step), open up `build/index.html` (**no server needed!**). Then modify whichever `.re` file in `src` and refresh the page to see the changes.

**For more elaborate ReasonReact examples**, please see https://github.com/reasonml-community/reason-react-example

## Run Project with Server

To run with the webpack development server run 

    npm run server
    
and view in the browser at:

  * http://localhost:8000.
  
Running in this environment provides hot reloading and support for routing; just edit and save the file and the browser will automatically refresh.

Note that any hot reload on a route will fall back to the root (`/`), so `ReasonReact.Router.dangerouslyGetInitialUrl` will likely be needed alongside the `ReasonReact.Router.watchUrl` logic to handle routing correctly on hot reload refreshes or simply opening the app at a URL that is not the root.

To use a port other than 8000 set the `PORT` environment variable (`PORT=8080 npm run server`).

## Build for Production

```sh
npm run clean
npm run build
npm run webpack:production
```

This will replace the development artifact `build/Index.js` for an optimized version as well as copy `src/index.html` into `build/`. You can then deploy the contents of the `build` directory (`index.html` and `Index.js`).

If you make use of routing (via `ReasonReact.Router` or similar logic) ensure that server-side routing handles your routes or that 404's are directed back to `index.html` (which is how the dev server is set up).

**To enable dead code elimination**, change `bsconfig.json`'s `package-specs` `module` from `"commonjs"` to `"es6"`. Then re-run the above 2 commands. This will allow Webpack to remove unused code.

## Background

I'm learning web technologies and playing with an idea of having a
nice web based game to learn basic music theory.

The ultimate goal of the game would be to in a fun and rewarding way teach the following:
  * Read all kinds of musical notations
  * Do musical "calculations"
  * Translate between numerical representations of various music theoretical concepts
  * Visualize some concepts from music theory in the minds eye
  * Do all the above  quickly and without using any tools or tables

I recognize that just one item from above list would be a worthy
goal for a hobby / learning project, but I don't think it hurts to have
lofty goals as well as learning is the goal and even a tiny bit of
success is considered worth celebrating.