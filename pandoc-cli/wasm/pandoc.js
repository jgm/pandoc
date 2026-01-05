/* pandoc.js: JavaScript interface to pandoc.wasm.
   Copyright (c) 2025 Tweag I/O Limited and John MacFarlane. MIT License.
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
export const fileSystem = new Map([
  ["stdin", in_file],
  ["stdout", out_file],
  ["stderr", err_file],
  ["warnings", warnings_file],
]);
const fds = [
  new OpenFile(new File(new Uint8Array(), { readonly: true })),
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
  new PreopenDirectory("/", fileSystem),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);
const { instance } = await WebAssembly.instantiateStreaming(
  fetch("./pandoc.wasm"),
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

export function pandoc(options, stdin) {
  const opts_str = JSON.stringify(options);
  const opts_ptr = instance.exports.malloc(opts_str.length);
  new TextEncoder().encodeInto(
    opts_str,
    new Uint8Array(instance.exports.memory.buffer, opts_ptr, opts_str.length)
  );
  // add output file if any
  if (options["output-file"]) {
    const file = new File("", { readonly: false });
    fileSystem.set(options["output-file"], file);
  }
  if (stdin) {
    in_file.data = new TextEncoder().encode(stdin);
  }
  instance.exports.wasm_main(opts_ptr, opts_str.length);
  return {
    stdout: new TextDecoder("utf-8", { fatal: true }).decode(out_file.data),
    stderr: new TextDecoder("utf-8", { fatal: true }).decode(err_file.data),
  };
}

export function addFile(filename, data) {
  const file = new File(data, { readonly: true });
  fileSystem.set(filename, file);
}
