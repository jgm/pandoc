/* pandoc.js: JavaScript interface to pandoc.wasm.
   Copyright (c) 2025 Tweag I/O Limited and John MacFarlane. MIT License.

   Interface:

   await convert(options, stdin, files)

   - options is a JavaScript object representing pandoc options: this should
     correspond to the format used in pandoc's default files.
   - stdin is a string or nil
   - files is a JavaScript object whose keys are filenames and whose values
     are the data in the corresponding file, as Blobs.

   The return value is a JavaScript object with 3 properties, stdout,
   stderr, and warnings, all strings. warnings is a JSON-encoded
   version of the warnings produced by pandoc. If the pandoc process
   produces an output file, it will be added to files.

   await query(options)

    - options is a JavaScript object with a 'query' property and in
      some cases a 'format' property. Possible queries include
      'version', 'highlight-styles', 'highlight-languages', 'input-formats',
      'output-formats', 'default-template' (requires 'format'),
      and 'extensions-for-format' (requires 'format').

   The return value is a JavaScript string or in some cases a list
   of strings.
*/

import {
  WASI,
  OpenFile,
  File,
  ConsoleStdout,
  PreopenDirectory,
} from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";

const args = ["pandoc.wasm", "+RTS", "-H64m", "-RTS"];
const env = [];
const in_file = new File(new Uint8Array(), { readonly: true });
const out_file = new File(new Uint8Array(), { readonly: false });
const err_file = new File(new Uint8Array(), { readonly: false });
const warnings_file = new File(new Uint8Array(), { readonly: false });
const fileSystem = new Map();
const fds = [
  new OpenFile(new File(new Uint8Array(), { readonly: true })),
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
  new PreopenDirectory("/", fileSystem),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);
const { instance } = await WebAssembly.instantiateStreaming(
  fetch("./pandoc.wasm?sha1=SHA1_PANDOC_WASM"),
  {
    wasi_snapshot_preview1: wasi.wasiImport,
  }
);

wasi.initialize(instance);
instance.exports.__wasm_call_ctors();

function memory_data_view() {
  return new DataView(instance.exports.memory.buffer);
}

const argc_ptr = instance.exports.malloc(4);
memory_data_view().setUint32(argc_ptr, args.length, true);
const argv = instance.exports.malloc(4 * (args.length + 1));
for (let i = 0; i < args.length; ++i) {
  const arg = instance.exports.malloc(args[i].length + 1);
  new TextEncoder().encodeInto(
    args[i],
    new Uint8Array(instance.exports.memory.buffer, arg, args[i].length)
  );
  memory_data_view().setUint8(arg + args[i].length, 0);
  memory_data_view().setUint32(argv + 4 * i, arg, true);
}
memory_data_view().setUint32(argv + 4 * args.length, 0, true);
const argv_ptr = instance.exports.malloc(4);
memory_data_view().setUint32(argv_ptr, argv, true);

instance.exports.hs_init_with_rtsopts(argc_ptr, argv_ptr);

export async function query(options) {
  const opts_str = JSON.stringify(options);
  const opts_bytes = new TextEncoder().encode(opts_str);
  const opts_ptr = instance.exports.malloc(opts_bytes.length);
  new Uint8Array(instance.exports.memory.buffer, opts_ptr, opts_bytes.length)
    .set(opts_bytes);
  // add input files to fileSystem
  fileSystem.clear()
  const out_file = new File(new Uint8Array(), { readonly: false });
  const err_file = new File(new Uint8Array(), { readonly: false });
  fileSystem.set("stdout", out_file);
  fileSystem.set("stderr", err_file);
  instance.exports.query(opts_ptr, opts_bytes.length);

  const err_text = new TextDecoder("utf-8", { fatal: true }).decode(err_file.data);
  if (err_text) console.log(err_text);
  const out_text = new TextDecoder("utf-8", { fatal: true }).decode(out_file.data);
  return JSON.parse(out_text);
}


export async function convert(options, stdin, files) {
  const opts_str = JSON.stringify(options);
  const opts_bytes = new TextEncoder().encode(opts_str);
  const opts_ptr = instance.exports.malloc(opts_bytes.length);
  new Uint8Array(instance.exports.memory.buffer, opts_ptr, opts_bytes.length)
    .set(opts_bytes);
  // add input files to fileSystem
  fileSystem.clear()
  const in_file = new File(new Uint8Array(), { readonly: true });
  const out_file = new File(new Uint8Array(), { readonly: false });
  const err_file = new File(new Uint8Array(), { readonly: false });
  const warnings_file = new File(new Uint8Array(), { readonly: false });
  fileSystem.set("stdin", in_file);
  fileSystem.set("stdout", out_file);
  fileSystem.set("stderr", err_file);
  fileSystem.set("warnings", warnings_file);
  for (const file in files) {
    await addFile(file, files[file], true);
  }
  // add output file if any
  if (options["output-file"]) {
    await addFile(options["output-file"], new Blob(), false);
  }
  // add media file for extracted media
  if (options["extract-media"]) {
    await addFile(options["extract-media"], new Blob(), false);
  }
  if (stdin) {
    in_file.data = new TextEncoder().encode(stdin);
  }
  instance.exports.convert(opts_ptr, opts_bytes.length);

  if (options["output-file"]) {
    files[options["output-file"]] =
       new Blob([fileSystem.get(options["output-file"]).data]);
  }
  if (options["extract-media"]) {
    const mediaFile = fileSystem.get(options["extract-media"]);
    if (mediaFile && mediaFile.data && mediaFile.data.length > 0) {
      files[options["extract-media"]] =
         new Blob([mediaFile.data], { type: 'application/zip' });
    }
  }
  const rawWarnings = new TextDecoder("utf-8", { fatal: true })
                          .decode(warnings_file.data);
  let warnings = [];
  if (rawWarnings) {
    warnings = JSON.parse(rawWarnings);
  }
  return {
    stdout: new TextDecoder("utf-8", { fatal: true }).decode(out_file.data),
    stderr: new TextDecoder("utf-8", { fatal: true }).decode(err_file.data),
    warnings: warnings
  };
}

async function addFile(filename, blob, readonly) {
  const buffer = await blob.arrayBuffer();
  const file = new File(new Uint8Array(buffer), { readonly: readonly });
  fileSystem.set(filename, file);
}
