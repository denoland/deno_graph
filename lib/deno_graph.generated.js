// @generated file from wasmbuild -- do not edit
// deno-lint-ignore-file
// deno-fmt-ignore-file
// source-hash: 11936c4bb11136732d5c33823d715d894a70c93a
let wasm;

const heap = new Array(32).fill(undefined);

heap.push(undefined, null, true, false);

function getObject(idx) {
  return heap[idx];
}

let WASM_VECTOR_LEN = 0;

let cachedUint8Memory0;
function getUint8Memory0() {
  if (cachedUint8Memory0.byteLength === 0) {
    cachedUint8Memory0 = new Uint8Array(wasm.memory.buffer);
  }
  return cachedUint8Memory0;
}

const cachedTextEncoder = new TextEncoder("utf-8");

const encodeString = function (arg, view) {
  return cachedTextEncoder.encodeInto(arg, view);
};

function passStringToWasm0(arg, malloc, realloc) {
  if (realloc === undefined) {
    const buf = cachedTextEncoder.encode(arg);
    const ptr = malloc(buf.length);
    getUint8Memory0().subarray(ptr, ptr + buf.length).set(buf);
    WASM_VECTOR_LEN = buf.length;
    return ptr;
  }

  let len = arg.length;
  let ptr = malloc(len);

  const mem = getUint8Memory0();

  let offset = 0;

  for (; offset < len; offset++) {
    const code = arg.charCodeAt(offset);
    if (code > 0x7F) break;
    mem[ptr + offset] = code;
  }

  if (offset !== len) {
    if (offset !== 0) {
      arg = arg.slice(offset);
    }
    ptr = realloc(ptr, len, len = offset + arg.length * 3);
    const view = getUint8Memory0().subarray(ptr + offset, ptr + len);
    const ret = encodeString(arg, view);

    offset += ret.written;
  }

  WASM_VECTOR_LEN = offset;
  return ptr;
}

let cachedInt32Memory0;
function getInt32Memory0() {
  if (cachedInt32Memory0.byteLength === 0) {
    cachedInt32Memory0 = new Int32Array(wasm.memory.buffer);
  }
  return cachedInt32Memory0;
}

let heap_next = heap.length;

function dropObject(idx) {
  if (idx < 36) return;
  heap[idx] = heap_next;
  heap_next = idx;
}

function takeObject(idx) {
  const ret = getObject(idx);
  dropObject(idx);
  return ret;
}

const cachedTextDecoder = new TextDecoder("utf-8", {
  ignoreBOM: true,
  fatal: true,
});

cachedTextDecoder.decode();

function getStringFromWasm0(ptr, len) {
  return cachedTextDecoder.decode(getUint8Memory0().subarray(ptr, ptr + len));
}

function addHeapObject(obj) {
  if (heap_next === heap.length) heap.push(heap.length + 1);
  const idx = heap_next;
  heap_next = heap[idx];

  heap[idx] = obj;
  return idx;
}

function debugString(val) {
  // primitive types
  const type = typeof val;
  if (type == "number" || type == "boolean" || val == null) {
    return `${val}`;
  }
  if (type == "string") {
    return `"${val}"`;
  }
  if (type == "symbol") {
    const description = val.description;
    if (description == null) {
      return "Symbol";
    } else {
      return `Symbol(${description})`;
    }
  }
  if (type == "function") {
    const name = val.name;
    if (typeof name == "string" && name.length > 0) {
      return `Function(${name})`;
    } else {
      return "Function";
    }
  }
  // objects
  if (Array.isArray(val)) {
    const length = val.length;
    let debug = "[";
    if (length > 0) {
      debug += debugString(val[0]);
    }
    for (let i = 1; i < length; i++) {
      debug += ", " + debugString(val[i]);
    }
    debug += "]";
    return debug;
  }
  // Test for built-in
  const builtInMatches = /\[object ([^\]]+)\]/.exec(toString.call(val));
  let className;
  if (builtInMatches.length > 1) {
    className = builtInMatches[1];
  } else {
    // Failed to match the standard '[object ClassName]'
    return toString.call(val);
  }
  if (className == "Object") {
    // we're a user defined class or Object
    // JSON.stringify avoids problems with cycles, and is generally much
    // easier than looping through ownProperties of `val`.
    try {
      return "Object(" + JSON.stringify(val) + ")";
    } catch (_) {
      return "Object";
    }
  }
  // errors
  if (val instanceof Error) {
    return `${val.name}: ${val.message}\n${val.stack}`;
  }
  // TODO we could test for more things here, like `Set`s and `Map`s.
  return className;
}

const CLOSURE_DTORS = new FinalizationRegistry((state) => {
  wasm.__wbindgen_export_2.get(state.dtor)(state.a, state.b);
});

function makeMutClosure(arg0, arg1, dtor, f) {
  const state = { a: arg0, b: arg1, cnt: 1, dtor };
  const real = (...args) => {
    // First up with a closure we increment the internal reference
    // count. This ensures that the Rust closure environment won't
    // be deallocated while we're invoking it.
    state.cnt++;
    const a = state.a;
    state.a = 0;
    try {
      return f(a, state.b, ...args);
    } finally {
      if (--state.cnt === 0) {
        wasm.__wbindgen_export_2.get(state.dtor)(a, state.b);
        CLOSURE_DTORS.unregister(state);
      } else {
        state.a = a;
      }
    }
  };
  real.original = state;
  CLOSURE_DTORS.register(real, state, state);
  return real;
}
function __wbg_adapter_20(arg0, arg1, arg2) {
  wasm
    ._dyn_core__ops__function__FnMut__A____Output___R_as_wasm_bindgen__closure__WasmClosure___describe__invoke__h66522527e9233982(
      arg0,
      arg1,
      addHeapObject(arg2),
    );
}

function isLikeNone(x) {
  return x === undefined || x === null;
}
/**
 * @param {string} specifier
 * @param {any} maybe_headers
 * @param {string | undefined} maybe_default_jsx_import_source
 * @param {string | undefined} maybe_jsx_import_source_module
 * @param {string} content
 * @param {any} maybe_kind
 * @param {Function | undefined} maybe_resolve
 * @param {Function | undefined} maybe_resolve_types
 * @returns {Module}
 */
export function parseModule(
  specifier,
  maybe_headers,
  maybe_default_jsx_import_source,
  maybe_jsx_import_source_module,
  content,
  maybe_kind,
  maybe_resolve,
  maybe_resolve_types,
) {
  try {
    const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
    const ptr0 = passStringToWasm0(
      specifier,
      wasm.__wbindgen_malloc,
      wasm.__wbindgen_realloc,
    );
    const len0 = WASM_VECTOR_LEN;
    var ptr1 = isLikeNone(maybe_default_jsx_import_source)
      ? 0
      : passStringToWasm0(
        maybe_default_jsx_import_source,
        wasm.__wbindgen_malloc,
        wasm.__wbindgen_realloc,
      );
    var len1 = WASM_VECTOR_LEN;
    var ptr2 = isLikeNone(maybe_jsx_import_source_module)
      ? 0
      : passStringToWasm0(
        maybe_jsx_import_source_module,
        wasm.__wbindgen_malloc,
        wasm.__wbindgen_realloc,
      );
    var len2 = WASM_VECTOR_LEN;
    const ptr3 = passStringToWasm0(
      content,
      wasm.__wbindgen_malloc,
      wasm.__wbindgen_realloc,
    );
    const len3 = WASM_VECTOR_LEN;
    wasm.parseModule(
      retptr,
      ptr0,
      len0,
      addHeapObject(maybe_headers),
      ptr1,
      len1,
      ptr2,
      len2,
      ptr3,
      len3,
      addHeapObject(maybe_kind),
      isLikeNone(maybe_resolve) ? 0 : addHeapObject(maybe_resolve),
      isLikeNone(maybe_resolve_types) ? 0 : addHeapObject(maybe_resolve_types),
    );
    var r0 = getInt32Memory0()[retptr / 4 + 0];
    var r1 = getInt32Memory0()[retptr / 4 + 1];
    var r2 = getInt32Memory0()[retptr / 4 + 2];
    if (r2) {
      throw takeObject(r1);
    }
    return Module.__wrap(r0);
  } finally {
    wasm.__wbindgen_add_to_stack_pointer(16);
  }
}

/**
 * @param {any} roots
 * @param {Function} load
 * @param {string | undefined} maybe_default_jsx_import_source
 * @param {string | undefined} maybe_jsx_import_source_module
 * @param {Function | undefined} maybe_cache_info
 * @param {Function | undefined} maybe_resolve
 * @param {Function | undefined} maybe_resolve_types
 * @param {string | undefined} maybe_build_kind
 * @param {any} maybe_imports
 * @returns {Promise<ModuleGraph>}
 */
export function createGraph(
  roots,
  load,
  maybe_default_jsx_import_source,
  maybe_jsx_import_source_module,
  maybe_cache_info,
  maybe_resolve,
  maybe_resolve_types,
  maybe_build_kind,
  maybe_imports,
) {
  var ptr0 = isLikeNone(maybe_default_jsx_import_source)
    ? 0
    : passStringToWasm0(
      maybe_default_jsx_import_source,
      wasm.__wbindgen_malloc,
      wasm.__wbindgen_realloc,
    );
  var len0 = WASM_VECTOR_LEN;
  var ptr1 = isLikeNone(maybe_jsx_import_source_module)
    ? 0
    : passStringToWasm0(
      maybe_jsx_import_source_module,
      wasm.__wbindgen_malloc,
      wasm.__wbindgen_realloc,
    );
  var len1 = WASM_VECTOR_LEN;
  var ptr2 = isLikeNone(maybe_build_kind)
    ? 0
    : passStringToWasm0(
      maybe_build_kind,
      wasm.__wbindgen_malloc,
      wasm.__wbindgen_realloc,
    );
  var len2 = WASM_VECTOR_LEN;
  const ret = wasm.createGraph(
    addHeapObject(roots),
    addHeapObject(load),
    ptr0,
    len0,
    ptr1,
    len1,
    isLikeNone(maybe_cache_info) ? 0 : addHeapObject(maybe_cache_info),
    isLikeNone(maybe_resolve) ? 0 : addHeapObject(maybe_resolve),
    isLikeNone(maybe_resolve_types) ? 0 : addHeapObject(maybe_resolve_types),
    ptr2,
    len2,
    addHeapObject(maybe_imports),
  );
  return takeObject(ret);
}

const u32CvtShim = new Uint32Array(2);

const uint64CvtShim = new BigUint64Array(u32CvtShim.buffer);

const int64CvtShim = new BigInt64Array(u32CvtShim.buffer);

function handleError(f, args) {
  try {
    return f.apply(this, args);
  } catch (e) {
    wasm.__wbindgen_exn_store(addHeapObject(e));
  }
}
function __wbg_adapter_65(arg0, arg1, arg2, arg3) {
  wasm.wasm_bindgen__convert__closures__invoke2_mut__h5566bdcbebef0dd9(
    arg0,
    arg1,
    addHeapObject(arg2),
    addHeapObject(arg3),
  );
}

const ModuleFinalization = new FinalizationRegistry((ptr) =>
  wasm.__wbg_module_free(ptr)
);
/** */
export class Module {
  static __wrap(ptr) {
    const obj = Object.create(Module.prototype);
    obj.ptr = ptr;
    ModuleFinalization.register(obj, obj.ptr, obj);
    return obj;
  }

  __destroy_into_raw() {
    const ptr = this.ptr;
    this.ptr = 0;
    ModuleFinalization.unregister(this);
    return ptr;
  }

  free() {
    const ptr = this.__destroy_into_raw();
    wasm.__wbg_module_free(ptr);
  }
  /**
   * @returns {any}
   */
  get cacheInfo() {
    const ret = wasm.module_cacheInfo(this.ptr);
    return takeObject(ret);
  }
  /**
   * @returns {string | undefined}
   */
  get checksum() {
    try {
      const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
      wasm.module_checksum(retptr, this.ptr);
      var r0 = getInt32Memory0()[retptr / 4 + 0];
      var r1 = getInt32Memory0()[retptr / 4 + 1];
      let v0;
      if (r0 !== 0) {
        v0 = getStringFromWasm0(r0, r1).slice();
        wasm.__wbindgen_free(r0, r1 * 1);
      }
      return v0;
    } finally {
      wasm.__wbindgen_add_to_stack_pointer(16);
    }
  }
  /**
   * @returns {any}
   */
  get dependencies() {
    const ret = wasm.module_dependencies(this.ptr);
    return takeObject(ret);
  }
  /**
   * @returns {any}
   */
  get kind() {
    const ret = wasm.module_kind(this.ptr);
    return takeObject(ret);
  }
  /**
   * @returns {string}
   */
  get mediaType() {
    try {
      const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
      wasm.module_mediaType(retptr, this.ptr);
      var r0 = getInt32Memory0()[retptr / 4 + 0];
      var r1 = getInt32Memory0()[retptr / 4 + 1];
      return getStringFromWasm0(r0, r1);
    } finally {
      wasm.__wbindgen_add_to_stack_pointer(16);
      wasm.__wbindgen_free(r0, r1);
    }
  }
  /**
   * @returns {number}
   */
  get size() {
    const ret = wasm.module_size(this.ptr);
    return ret >>> 0;
  }
  /**
   * @returns {string}
   */
  get source() {
    try {
      const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
      wasm.module_source(retptr, this.ptr);
      var r0 = getInt32Memory0()[retptr / 4 + 0];
      var r1 = getInt32Memory0()[retptr / 4 + 1];
      return getStringFromWasm0(r0, r1);
    } finally {
      wasm.__wbindgen_add_to_stack_pointer(16);
      wasm.__wbindgen_free(r0, r1);
    }
  }
  /**
   * @returns {string}
   */
  get specifier() {
    try {
      const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
      wasm.module_specifier(retptr, this.ptr);
      var r0 = getInt32Memory0()[retptr / 4 + 0];
      var r1 = getInt32Memory0()[retptr / 4 + 1];
      return getStringFromWasm0(r0, r1);
    } finally {
      wasm.__wbindgen_add_to_stack_pointer(16);
      wasm.__wbindgen_free(r0, r1);
    }
  }
  /**
   * @returns {any}
   */
  get typesDependency() {
    const ret = wasm.module_typesDependency(this.ptr);
    return takeObject(ret);
  }
  /**
   * @returns {any}
   */
  toJSON() {
    const ret = wasm.module_toJSON(this.ptr);
    return takeObject(ret);
  }
}

const ModuleGraphFinalization = new FinalizationRegistry((ptr) =>
  wasm.__wbg_modulegraph_free(ptr)
);
/** */
export class ModuleGraph {
  static __wrap(ptr) {
    const obj = Object.create(ModuleGraph.prototype);
    obj.ptr = ptr;
    ModuleGraphFinalization.register(obj, obj.ptr, obj);
    return obj;
  }

  __destroy_into_raw() {
    const ptr = this.ptr;
    this.ptr = 0;
    ModuleGraphFinalization.unregister(this);
    return ptr;
  }

  free() {
    const ptr = this.__destroy_into_raw();
    wasm.__wbg_modulegraph_free(ptr);
  }
  /**
   * @returns {Array<any>}
   */
  get roots() {
    const ret = wasm.modulegraph_roots(this.ptr);
    return takeObject(ret);
  }
  /**
   * @param {string} specifier
   * @returns {Module | undefined}
   */
  get(specifier) {
    try {
      const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
      const ptr0 = passStringToWasm0(
        specifier,
        wasm.__wbindgen_malloc,
        wasm.__wbindgen_realloc,
      );
      const len0 = WASM_VECTOR_LEN;
      wasm.modulegraph_get(retptr, this.ptr, ptr0, len0);
      var r0 = getInt32Memory0()[retptr / 4 + 0];
      var r1 = getInt32Memory0()[retptr / 4 + 1];
      var r2 = getInt32Memory0()[retptr / 4 + 2];
      if (r2) {
        throw takeObject(r1);
      }
      return r0 === 0 ? undefined : Module.__wrap(r0);
    } finally {
      wasm.__wbindgen_add_to_stack_pointer(16);
    }
  }
  /**
   * @returns {Array<any>}
   */
  get modules() {
    const ret = wasm.modulegraph_modules(this.ptr);
    return takeObject(ret);
  }
  /**
   * @param {string} specifier
   * @returns {string}
   */
  resolve(specifier) {
    try {
      const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
      const ptr0 = passStringToWasm0(
        specifier,
        wasm.__wbindgen_malloc,
        wasm.__wbindgen_realloc,
      );
      const len0 = WASM_VECTOR_LEN;
      wasm.modulegraph_resolve(retptr, this.ptr, ptr0, len0);
      var r0 = getInt32Memory0()[retptr / 4 + 0];
      var r1 = getInt32Memory0()[retptr / 4 + 1];
      return getStringFromWasm0(r0, r1);
    } finally {
      wasm.__wbindgen_add_to_stack_pointer(16);
      wasm.__wbindgen_free(r0, r1);
    }
  }
  /**
   * @param {string} specifier
   * @param {string} referrer
   * @param {boolean} prefer_types
   * @returns {string | undefined}
   */
  resolveDependency(specifier, referrer, prefer_types) {
    try {
      const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
      const ptr0 = passStringToWasm0(
        specifier,
        wasm.__wbindgen_malloc,
        wasm.__wbindgen_realloc,
      );
      const len0 = WASM_VECTOR_LEN;
      const ptr1 = passStringToWasm0(
        referrer,
        wasm.__wbindgen_malloc,
        wasm.__wbindgen_realloc,
      );
      const len1 = WASM_VECTOR_LEN;
      wasm.modulegraph_resolveDependency(
        retptr,
        this.ptr,
        ptr0,
        len0,
        ptr1,
        len1,
        prefer_types,
      );
      var r0 = getInt32Memory0()[retptr / 4 + 0];
      var r1 = getInt32Memory0()[retptr / 4 + 1];
      let v2;
      if (r0 !== 0) {
        v2 = getStringFromWasm0(r0, r1).slice();
        wasm.__wbindgen_free(r0, r1 * 1);
      }
      return v2;
    } finally {
      wasm.__wbindgen_add_to_stack_pointer(16);
    }
  }
  /**
   * @returns {any}
   */
  toJSON() {
    const ret = wasm.modulegraph_toJSON(this.ptr);
    return takeObject(ret);
  }
}

const imports = {
  __wbindgen_placeholder__: {
    __wbindgen_json_serialize: function (arg0, arg1) {
      const obj = getObject(arg1);
      const ret = JSON.stringify(obj === undefined ? null : obj);
      const ptr0 = passStringToWasm0(
        ret,
        wasm.__wbindgen_malloc,
        wasm.__wbindgen_realloc,
      );
      const len0 = WASM_VECTOR_LEN;
      getInt32Memory0()[arg0 / 4 + 1] = len0;
      getInt32Memory0()[arg0 / 4 + 0] = ptr0;
    },
    __wbindgen_object_drop_ref: function (arg0) {
      takeObject(arg0);
    },
    __wbindgen_string_new: function (arg0, arg1) {
      const ret = getStringFromWasm0(arg0, arg1);
      return addHeapObject(ret);
    },
    __wbg_module_new: function (arg0) {
      const ret = Module.__wrap(arg0);
      return addHeapObject(ret);
    },
    __wbg_modulegraph_new: function (arg0) {
      const ret = ModuleGraph.__wrap(arg0);
      return addHeapObject(ret);
    },
    __wbindgen_object_clone_ref: function (arg0) {
      const ret = getObject(arg0);
      return addHeapObject(ret);
    },
    __wbindgen_number_new: function (arg0) {
      const ret = arg0;
      return addHeapObject(ret);
    },
    __wbg_BigInt_1b7cf17b993da2bd: function (arg0, arg1) {
      u32CvtShim[0] = arg0;
      u32CvtShim[1] = arg1;
      const n0 = uint64CvtShim[0];
      const ret = BigInt(n0);
      return addHeapObject(ret);
    },
    __wbg_BigInt_4f2945f00df26418: function (arg0, arg1) {
      u32CvtShim[0] = arg0;
      u32CvtShim[1] = arg1;
      const n0 = int64CvtShim[0];
      const ret = BigInt(n0);
      return addHeapObject(ret);
    },
    __wbg_set_fbb49ad265f9dee8: function (arg0, arg1, arg2) {
      getObject(arg0)[takeObject(arg1)] = takeObject(arg2);
    },
    __wbindgen_cb_drop: function (arg0) {
      const obj = takeObject(arg0).original;
      if (obj.cnt-- == 1) {
        obj.a = 0;
        return true;
      }
      const ret = false;
      return ret;
    },
    __wbindgen_is_string: function (arg0) {
      const ret = typeof (getObject(arg0)) === "string";
      return ret;
    },
    __wbg_new_2ab697f1555e0dbc: function () {
      const ret = new Array();
      return addHeapObject(ret);
    },
    __wbg_new_f0540490a24a79bb: function () {
      const ret = new Map();
      return addHeapObject(ret);
    },
    __wbg_new_306ce8d57919e6ae: function () {
      const ret = new Object();
      return addHeapObject(ret);
    },
    __wbg_push_811c8b08bf4ff9d5: function (arg0, arg1) {
      const ret = getObject(arg0).push(getObject(arg1));
      return ret;
    },
    __wbg_new_651776e932b7e9c7: function (arg0, arg1) {
      const ret = new Error(getStringFromWasm0(arg0, arg1));
      return addHeapObject(ret);
    },
    __wbg_call_9855a4612eb496cb: function () {
      return handleError(function (arg0, arg1, arg2) {
        const ret = getObject(arg0).call(getObject(arg1), getObject(arg2));
        return addHeapObject(ret);
      }, arguments);
    },
    __wbg_call_8e1338b908441bd2: function () {
      return handleError(function (arg0, arg1, arg2, arg3) {
        const ret = getObject(arg0).call(
          getObject(arg1),
          getObject(arg2),
          getObject(arg3),
        );
        return addHeapObject(ret);
      }, arguments);
    },
    __wbg_set_e44d2fe94a18567f: function (arg0, arg1, arg2) {
      const ret = getObject(arg0).set(getObject(arg1), getObject(arg2));
      return addHeapObject(ret);
    },
    __wbg_new_78403b138428b684: function (arg0, arg1) {
      try {
        var state0 = { a: arg0, b: arg1 };
        var cb0 = (arg0, arg1) => {
          const a = state0.a;
          state0.a = 0;
          try {
            return __wbg_adapter_65(a, state0.b, arg0, arg1);
          } finally {
            state0.a = a;
          }
        };
        const ret = new Promise(cb0);
        return addHeapObject(ret);
      } finally {
        state0.a = state0.b = 0;
      }
    },
    __wbg_resolve_f269ce174f88b294: function (arg0) {
      const ret = Promise.resolve(getObject(arg0));
      return addHeapObject(ret);
    },
    __wbg_then_1c698eedca15eed6: function (arg0, arg1) {
      const ret = getObject(arg0).then(getObject(arg1));
      return addHeapObject(ret);
    },
    __wbg_then_4debc41d4fc92ce5: function (arg0, arg1, arg2) {
      const ret = getObject(arg0).then(getObject(arg1), getObject(arg2));
      return addHeapObject(ret);
    },
    __wbindgen_debug_string: function (arg0, arg1) {
      const ret = debugString(getObject(arg1));
      const ptr0 = passStringToWasm0(
        ret,
        wasm.__wbindgen_malloc,
        wasm.__wbindgen_realloc,
      );
      const len0 = WASM_VECTOR_LEN;
      getInt32Memory0()[arg0 / 4 + 1] = len0;
      getInt32Memory0()[arg0 / 4 + 0] = ptr0;
    },
    __wbindgen_throw: function (arg0, arg1) {
      throw new Error(getStringFromWasm0(arg0, arg1));
    },
    __wbindgen_closure_wrapper1574: function (arg0, arg1, arg2) {
      const ret = makeMutClosure(arg0, arg1, 220, __wbg_adapter_20);
      return addHeapObject(ret);
    },
  },
};

const wasm_url = new URL("deno_graph_bg.wasm", import.meta.url);

/**
 * Decompression callback
 *
 * @callback decompressCallback
 * @param {Uint8Array} compressed
 * @return {Uint8Array} decompressed
 */

/** Instantiates an instance of the Wasm module returning its functions.
 * @remarks It is safe to call this multiple times and once successfully
 * loaded it will always return a reference to the same object.
 * @param {decompressCallback=} transform
 */
export async function instantiate(transform) {
  return (await instantiateWithInstance(transform)).exports;
}

let instanceWithExports;
let lastLoadPromise;

/** Instantiates an instance of the Wasm module along with its exports.
 * @remarks It is safe to call this multiple times and once successfully
 * loaded it will always return a reference to the same object.
 * @param {decompressCallback=} transform
 * @returns {Promise<{
 *   instance: WebAssembly.Instance;
 *   exports: { parseModule: typeof parseModule; createGraph: typeof createGraph; Module : typeof Module ; ModuleGraph : typeof ModuleGraph  }
 * }>}
 */
export function instantiateWithInstance(transform) {
  if (instanceWithExports != null) {
    return Promise.resolve(instanceWithExports);
  }
  if (lastLoadPromise == null) {
    lastLoadPromise = (async () => {
      try {
        const instance = (await instantiateModule(transform)).instance;
        wasm = instance.exports;
        cachedInt32Memory0 = new Int32Array(wasm.memory.buffer);
        cachedUint8Memory0 = new Uint8Array(wasm.memory.buffer);
        instanceWithExports = {
          instance,
          exports: getWasmInstanceExports(),
        };
        return instanceWithExports;
      } finally {
        lastLoadPromise = null;
      }
    })();
  }
  return lastLoadPromise;
}

function getWasmInstanceExports() {
  return { parseModule, createGraph, Module, ModuleGraph };
}

/** Gets if the Wasm module has been instantiated. */
export function isInstantiated() {
  return instanceWithExports != null;
}

async function instantiateModule(transform) {
  switch (wasm_url.protocol) {
    case "file:": {
      if (typeof Deno !== "object") {
        throw new Error("file urls are not supported in this environment");
      }

      if ("permissions" in Deno) {
        await Deno.permissions.request({ name: "read", path: wasm_url });
      }
      const wasmCode = await Deno.readFile(wasm_url);
      return WebAssembly.instantiate(
        !transform ? wasmCode : transform(wasmCode),
        imports,
      );
    }
    case "https:":
    case "http:": {
      if (typeof Deno === "object" && "permissions" in Deno) {
        await Deno.permissions.request({ name: "net", host: wasm_url.host });
      }
      const wasmResponse = await fetch(wasm_url);
      if (transform) {
        const wasmCode = new Uint8Array(await wasmResponse.arrayBuffer());
        return WebAssembly.instantiate(transform(wasmCode), imports);
      }
      if (
        wasmResponse.headers.get("content-type")?.toLowerCase().startsWith(
          "application/wasm",
        )
      ) {
        return WebAssembly.instantiateStreaming(wasmResponse, imports);
      } else {
        return WebAssembly.instantiate(
          await wasmResponse.arrayBuffer(),
          imports,
        );
      }
    }
    default:
      throw new Error(`Unsupported protocol: ${wasm_url.protocol}`);
  }
}
