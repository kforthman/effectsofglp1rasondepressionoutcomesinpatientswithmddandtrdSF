"""Generate pipeline_map.csv by static analysis of the R scripts."""
import re, csv, os

THIS_SCRIPT = os.path.basename(__file__)
CSV_SCRIPT  = "pipeline_map.R"

# ── helpers ──────────────────────────────────────────────────────────────────

def strip_comments(lines):
    out = []
    for l in lines:
        l = l.rstrip('\n')
        # Drop full-line comments
        if re.match(r'^\s*#', l):
            continue
        # Strip inline comments: walk char-by-char to find # outside strings
        result = []
        in_dq = in_sq = False
        for ch in l:
            if ch == '"' and not in_sq:
                in_dq = not in_dq
            elif ch == "'" and not in_dq:
                in_sq = not in_sq
            elif ch == '#' and not in_dq and not in_sq:
                break  # rest of line is a comment
            result.append(ch)
        out.append(''.join(result))
    return out


def extract_quoted(line):
    results = []
    results += re.findall(r'"([^"]*)"', line)
    results += re.findall(r"'([^']*)'", line)
    return results


def is_file_like(s):
    """True for complete filenames (or patterns with {var} placeholders).
    Rejects bare extensions like .csv and paste0-fragment suffixes like _foo.rds."""
    if re.match(r'^https?://', s):
        return False
    m = re.match(r'^(.+?)(\.[a-zA-Z0-9]+)$', s)
    if not m:
        return False
    stem = m.group(1)
    # Stem must start with an alphanumeric character or '{' (template placeholder).
    # This filters out suffix fragments extracted from inside paste0() calls,
    # e.g. "_index_info.rds" from paste0(this_drug, "_index_info.rds").
    return bool(re.match(r'^[a-zA-Z0-9{]', stem))


def parse_paste0_args(content):
    """Split paste0 content by top-level commas."""
    args, current, depth = [], [], 0
    in_dq = in_sq = False
    for ch in content:
        if   ch == '"' and not in_sq: in_dq = not in_dq; current.append(ch)
        elif ch == "'" and not in_dq: in_sq = not in_sq; current.append(ch)
        elif not in_dq and not in_sq:
            if   ch in ('(', '[', '{'): depth += 1; current.append(ch)
            elif ch in (')', ']', '}'): depth -= 1; current.append(ch)
            elif ch == ',' and depth == 0:
                args.append(''.join(current).strip()); current = []
            else: current.append(ch)
        else: current.append(ch)
    if current:
        args.append(''.join(current).strip())
    return args


def reconstruct_paste0(content):
    """Reconstruct a filename pattern from the arguments inside paste0(...)."""
    parts = []
    for arg in parse_paste0_args(content):
        m = re.match(r'^"([^"]*)"$', arg)
        if m: parts.append(m.group(1)); continue
        m = re.match(r"^'([^']*)'$", arg)
        if m: parts.append(m.group(1)); continue
        if re.match(r'^[\d.]+$', arg): parts.append(arg); continue
        parts.append('{' + arg + '}')
    return ''.join(parts)


def extract_paste0_names(line):
    """Find every paste0(...) call and return reconstructed filename patterns."""
    results = []
    pos = 0
    while pos < len(line):
        m = re.search(r'\bpaste0\s*\(', line[pos:])
        if not m:
            break
        open_pos = pos + m.end()   # first char inside the (
        depth = 1
        i = open_pos
        while i < len(line) and depth > 0:
            if   line[i] == '(': depth += 1
            elif line[i] == ')': depth -= 1
            i += 1
        content = line[open_pos:i - 1]
        reconstructed = reconstruct_paste0(content)
        if is_file_like(reconstructed):
            results.append(reconstructed)
        pos = i
    return results


def extract_var_assignments(lines):
    """Return {varname: [file_path, ...]} collecting ALL assignments per variable.
    Handles cases like `filename <- "a.rds"` ... `filename <- "b.rds"` where both
    values must be resolved when `filename` appears in a save/load call."""
    result = {}
    for ln in lines:
        if not re.search(r'<-\s*["\']', ln):
            continue
        m = re.match(r'^\s*(\w+)\s*<-.*', ln)
        if not m:
            continue
        vname = m.group(1)
        vals = [v for v in extract_quoted(ln) if is_file_like(v)]
        if len(vals) == 1:
            result.setdefault(vname, [])
            if vals[0] not in result[vname]:
                result[vname].append(vals[0])
    return result


def extract_info_df_file_paths(lines):
    """Find variables assigned as c("label","path",...) %>% matrix(ncol=2,byrow=TRUE).
    Returns dict: {varname -> [file_path, ...]} with every other quoted string as a path."""
    text = '\n'.join(lines)
    result = {}
    for m in re.finditer(r'\b(\w+)\s*<-\s*c\s*\(', text):
        varname = m.group(1)
        start = m.end()
        depth, i, in_dq, in_sq = 1, start, False, False
        while i < len(text) and depth > 0:
            ch = text[i]
            if   ch == '"' and not in_sq: in_dq = not in_dq
            elif ch == "'" and not in_dq: in_sq = not in_sq
            elif not in_dq and not in_sq:
                if   ch == '(': depth += 1
                elif ch == ')': depth -= 1
            i += 1
        c_content = text[start:i - 1]
        after_c   = text[i:i + 300]
        if not re.search(r'matrix\s*\(\s*ncol\s*=\s*2', after_c):
            continue
        all_strings = re.findall(r'"([^"]*)"', c_content)
        # Odd-indexed entries (0-based: 1, 3, 5, ...) are file paths
        file_paths = [s for j, s in enumerate(all_strings) if j % 2 == 1 and is_file_like(s)]
        if file_paths:
            result[varname] = file_paths
    return result


def _extract_paste0_from_file_exists(l, pattern_prefix):
    """Helper: extract paste0(...) content from file.exists(paste0(...)) lines.
    pattern_prefix is the regex prefix to locate file.exists, e.g. r'!file\.exists' or r'\bfile\.exists'.
    Returns a list of reconstructed filename patterns."""
    results = []
    for m in re.finditer(pattern_prefix + r'\s*\(\s*paste0\s*\(', l):
        idx = l.index('paste0', m.start()) + len('paste0')
        while idx < len(l) and l[idx] != '(':
            idx += 1
        if idx >= len(l):
            continue
        open_pos = idx + 1
        depth, i, in_dq, in_sq = 1, open_pos, False, False
        while i < len(l) and depth > 0:
            ch = l[i]
            if   ch == '"' and not in_sq: in_dq = not in_dq
            elif ch == "'" and not in_dq: in_sq = not in_sq
            elif not in_dq and not in_sq:
                if   ch == '(': depth += 1
                elif ch == ')': depth -= 1
            i += 1
        content = l[open_pos:i - 1]
        reconstructed = reconstruct_paste0(content)
        if reconstructed:
            results.append(reconstructed)
    return results


def extract_not_file_exists_files(lines, var_assignments):
    """Return a set of filename patterns that appear inside create-if-not-exists guards.

    Handles these forms:
      !file.exists("literal.ext")          – standard guard
      !file.exists(paste0(...))            – standard guard with paste0
      !file.exists(some_variable)          – standard guard with variable
      file.exists("literal.ext") & !overwrite   – inverse guard (load if exists)
      file.exists(paste0(...))   & !overwrite   – inverse guard with paste0
      file.exists(some_variable) & !overwrite   – inverse guard with variable
    """
    guarded = set()
    for l in lines:
        # ── Standard !file.exists(...) guards ─────────────────────────────────

        # Literal string: !file.exists("foo.csv")
        for m in re.finditer(r'!file\.exists\s*\(\s*["\']([^"\']+)["\']', l):
            fname = m.group(1)
            if is_file_like(fname):
                guarded.add(fname)

        # paste0 call: !file.exists(paste0(...))
        for fname in _extract_paste0_from_file_exists(l, r'!file\.exists'):
            guarded.add(fname)

        # Variable reference: !file.exists(varname) or !file.exists(varname[...])
        for m in re.finditer(r'!file\.exists\s*\(\s*(\w+)\s*[\)\[,|&]', l):
            vname = m.group(1)
            if vname in var_assignments:
                guarded.update(var_assignments[vname])

        # ── Inverse guard: file.exists(X) & !overwrite ───────────────────────
        # Pattern: if(file.exists(X) & !overwrite){ load(X) } else { save(X) }
        # The file is self-created (written when new, read when existing).
        if re.search(r'(?:&\s*!overwrite|!overwrite\s*&)', l):
            # Literal string
            for m in re.finditer(r'\bfile\.exists\s*\(\s*["\']([^"\']+)["\']', l):
                fname = m.group(1)
                if is_file_like(fname):
                    guarded.add(fname)

            # paste0 call: file.exists(paste0(...)) & !overwrite
            for fname in _extract_paste0_from_file_exists(l, r'\bfile\.exists'):
                guarded.add(fname)

            # Variable reference: file.exists(varname) & !overwrite
            for m in re.finditer(r'\bfile\.exists\s*\(\s*(\w+)\s*[\)\[,|&]', l):
                vname = m.group(1)
                if vname in var_assignments:
                    guarded.update(var_assignments[vname])

    return guarded


def resolve_vars(io_lines, var_assignments):
    if not io_lines or not var_assignments:
        return []
    resolved = []
    for vname, fnames in var_assignments.items():
        pattern = r'\b' + re.escape(vname) + r'\b'
        if any(re.search(pattern, l) for l in io_lines):
            resolved.extend(fnames)
    return resolved


def resolve_df_col_refs(io_lines, info_df_paths):
    """Resolve varname$file_path[i] patterns in I/O lines."""
    resolved = []
    for line in io_lines:
        for m in re.finditer(r'\b(\w+)\s*\$\s*file_path\b', line):
            vname = m.group(1)
            if vname in info_df_paths:
                resolved.extend(info_df_paths[vname])
    return resolved


# ── placeholder expansion helpers ────────────────────────────────────────────

def extract_string_var_values(lines):
    """Collect {varname: [val, ...]} for all simple `var <- "string"` assignments."""
    result = {}
    for ln in lines:
        m = re.match(r'^\s*(\w+)\s*<-\s*"([^"]*)"', ln)
        if not m:
            m = re.match(r"^\s*(\w+)\s*<-\s*'([^']*)'", ln)
        if m:
            vname, val = m.group(1), m.group(2)
            result.setdefault(vname, [])
            if val not in result[vname]:
                result[vname].append(val)
    return result


def extract_for_loop_int_ranges(lines):
    """Extract {varname: ['1','2',...]} from `for(var in start:end)` patterns."""
    result = {}
    for ln in lines:
        m = re.search(r'\bfor\s*\(\s*(\w+)\s+in\s+(\d+)\s*:\s*(\d+)\s*\)', ln)
        if m:
            vname = m.group(1)
            result[vname] = [str(n) for n in range(int(m.group(2)), int(m.group(3)) + 1)]
    return result


def extract_df_column_values(lines):
    """For c(...) %>% matrix(ncol=N) data frames, return all column values keyed
    by (varname, col_name).  Handles ncol=1 and ncol=2."""
    text = '\n'.join(lines)
    result = {}
    for m in re.finditer(r'\b(\w+)\s*<-\s*c\s*\(', text):
        varname = m.group(1)
        start = m.end()
        depth, i, in_dq, in_sq = 1, start, False, False
        while i < len(text) and depth > 0:
            ch = text[i]
            if   ch == '"' and not in_sq: in_dq = not in_dq
            elif ch == "'" and not in_dq: in_sq = not in_sq
            elif not in_dq and not in_sq:
                if   ch == '(': depth += 1
                elif ch == ')': depth -= 1
            i += 1
        c_content = text[start:i - 1]
        after_c   = text[i:i + 300]
        ncol_m = re.search(r'matrix\s*\(\s*ncol\s*=\s*(\d+)', after_c)
        if not ncol_m:
            continue
        ncol = int(ncol_m.group(1))
        names_m = re.search(r'setNames\s*\(\s*c\s*\(([^)]+)\)', after_c)
        col_names = re.findall(r'"([^"]+)"', names_m.group(1)) if names_m else []
        all_strings = re.findall(r'"([^"]*)"', c_content)
        for col_idx in range(ncol):
            col_vals = [s for j, s in enumerate(all_strings) if j % ncol == col_idx]
            col_name = col_names[col_idx] if col_idx < len(col_names) else f'col{col_idx+1}'
            result[(varname, col_name)] = col_vals
    return result


def extract_loop_var_values(lines, df_col_values):
    """Detect `varname <- dfname$colname[i]` and look up the column values."""
    result = {}
    for ln in lines:
        m = re.match(r'^\s*(\w+)\s*<-\s+(\w+)\s*\$\s*(\w+)\s*\[', ln)
        if m:
            vname, df_name, col_name = m.group(1), m.group(2), m.group(3)
            key = (df_name, col_name)
            if key in df_col_values:
                vals = [v for v in df_col_values[key] if v and not is_file_like(v)]
                if vals:
                    result[vname] = vals
    return result


def resolve_paste0_var_values(lines, all_var_values):
    """Evaluate `varname <- paste0(other_var, "_suffix")` using known var values.
    Returns {varname: [expanded_val, ...]}."""
    result = {}
    for ln in lines:
        m = re.match(r'^\s*(\w+)\s*<-\s*paste0\s*\(', ln)
        if not m:
            continue
        vname = m.group(1)
        open_pos = ln.index('(', m.end() - 1) + 1
        depth, i, in_dq, in_sq = 1, open_pos, False, False
        while i < len(ln) and depth > 0:
            ch = ln[i]
            if   ch == '"' and not in_sq: in_dq = not in_dq
            elif ch == "'" and not in_dq: in_sq = not in_sq
            elif not in_dq and not in_sq:
                if   ch == '(': depth += 1
                elif ch == ')': depth -= 1
            i += 1
        content = ln[open_pos:i - 1]
        reconstructed = reconstruct_paste0(content)
        vars_in_rec = re.findall(r'\{(\w+)\}', reconstructed)
        if len(vars_in_rec) == 1:
            v = vars_in_rec[0]
            if v in all_var_values:
                result[vname] = [reconstructed.replace('{' + v + '}', val)
                                 for val in all_var_values[v]]
        elif len(vars_in_rec) == 0 and reconstructed:
            result[vname] = [reconstructed]
    return result


def _find_all_adjacent_pairs(lines):
    """Find `varA <- "X"` / `varB <- "Y"` pairs where both appear within 4 lines
    of each other (possibly with intervening assignments in between).
    Returns {(vA, vB): [(valA, valB), ...]}."""
    pairs = {}
    for i in range(len(lines)):
        m1 = re.match(r'^\s*(\w+)\s*<-\s*"([^"]*)"', lines[i])
        if not m1:
            continue
        v1, val1 = m1.group(1), m1.group(2)
        for j in range(i + 1, min(i + 5, len(lines))):
            m2 = re.match(r'^\s*(\w+)\s*<-\s*"([^"]*)"', lines[j])
            if m2:
                v2, val2 = m2.group(1), m2.group(2)
                if v1 != v2:
                    key = (v1, v2)
                    pairs.setdefault(key, [])
                    if (val1, val2) not in pairs[key]:
                        pairs[key].append((val1, val2))
                # continue scanning - don't break on intervening assignments
    return pairs


def expand_placeholders(patterns, all_var_values, adjacent_pairs):
    """Expand {var} placeholders in filename patterns into actual filenames."""
    result = []
    for pat in patterns:
        vars_in_pat = list(dict.fromkeys(re.findall(r'\{(\w+)\}', pat)))
        if not vars_in_pat:
            result.append(pat)
            continue

        known = [v for v in vars_in_pat if all_var_values.get(v)]
        if not known:
            result.append(pat)
            continue

        if len(vars_in_pat) == 1:
            v = vars_in_pat[0]
            vals = all_var_values.get(v, [])
            if not vals:
                result.append(pat)
                continue

            # If the pattern embeds a literal that is a known value of another variable,
            # restrict v's values to only those adjacent to that literal.
            filtered = None
            for other_var, other_vals in all_var_values.items():
                if other_var == v:
                    continue
                for other_val in other_vals:
                    sep = r'[-/.]'
                    if re.search(sep + re.escape(other_val) + sep, pat) or \
                       re.search(sep + re.escape(other_val) + r'$', pat):
                        for key in ((v, other_var), (other_var, v)):
                            if key in adjacent_pairs:
                                idx = 0 if key[0] == v else 1
                                cands = [p[idx] for p in adjacent_pairs[key]
                                         if p[1 - idx] == other_val]
                                if cands:
                                    filtered = cands
                                    break
                        if filtered:
                            break
                if filtered:
                    break

            for val in (filtered if filtered is not None else vals):
                result.append(pat.replace('{' + v + '}', val))

        elif len(vars_in_pat) == 2:
            v1, v2 = vars_in_pat[0], vars_in_pat[1]
            key1, key2 = (v1, v2), (v2, v1)
            # When all_var_values is restricted (e.g. call-site override), filter
            # adjacent_pairs to only include pairs whose values are allowed.
            allowed1 = set(all_var_values.get(v1, []))
            allowed2 = set(all_var_values.get(v2, []))
            def _filter_pairs(pairs_list, a_set, b_set):
                return [(a, b) for a, b in pairs_list
                        if (not a_set or a in a_set) and (not b_set or b in b_set)]
            if key1 in adjacent_pairs:
                use_pairs = _filter_pairs(adjacent_pairs[key1], allowed1, allowed2)
                for val1, val2 in use_pairs:
                    result.append(pat.replace('{' + v1 + '}', val1)
                                     .replace('{' + v2 + '}', val2))
                if not use_pairs:
                    # No valid adjacent pairs after filtering; fall back to cartesian
                    vals1 = list(allowed1) or all_var_values.get(v1, [])
                    vals2 = list(allowed2) or all_var_values.get(v2, [])
                    for val1 in vals1:
                        for val2 in vals2:
                            result.append(pat.replace('{' + v1 + '}', val1)
                                             .replace('{' + v2 + '}', val2))
            elif key2 in adjacent_pairs:
                use_pairs = _filter_pairs(adjacent_pairs[key2], allowed2, allowed1)
                for val2, val1 in use_pairs:
                    result.append(pat.replace('{' + v1 + '}', val1)
                                     .replace('{' + v2 + '}', val2))
                if not use_pairs:
                    vals1 = list(allowed1) or all_var_values.get(v1, [])
                    vals2 = list(allowed2) or all_var_values.get(v2, [])
                    for val1 in vals1:
                        for val2 in vals2:
                            result.append(pat.replace('{' + v1 + '}', val1)
                                             .replace('{' + v2 + '}', val2))
            else:
                vals1 = all_var_values.get(v1, [])
                vals2 = all_var_values.get(v2, [])
                if vals1 and vals2:
                    for val1 in vals1:
                        for val2 in vals2:
                            result.append(pat.replace('{' + v1 + '}', val1)
                                             .replace('{' + v2 + '}', val2))
                else:
                    result.append(pat)
        else:
            result.append(pat)

    return result


def extract_local_functions(lines):
    """Detect locally-defined functions.
    Returns {func_name: {'params': [...], 'body': set(line_indices)}}.
    Line indices are the lines INSIDE the function body (not the definition line itself).
    Handles multi-line parameter lists (e.g. function definition that wraps to the next line).
    """
    funcs = {}
    i = 0
    n = len(lines)
    while i < n:
        m = re.match(r'^\s*(\w+)\s*<-\s*function\s*\(', lines[i])
        if m:
            func_name = m.group(1)
            # Collect parameter text across potentially multiple lines until the
            # matching closing ')' of the parameter list is found.
            open_pos = lines[i].index('(', m.end() - 1)
            params_text = ''
            depth = 0
            found_end = False
            for k in range(i, min(i + 20, n)):
                start = open_pos + 1 if k == i else 0
                seg = lines[k][start:]
                for ci, ch in enumerate(seg):
                    if ch == '(':
                        depth += 1
                    elif ch == ')':
                        if depth == 0:
                            params_text += seg[:ci]
                            found_end = True
                            break
                        depth -= 1
                if found_end:
                    break
                params_text += seg
            params = [p.strip().split('=')[0].strip()
                      for p in params_text.split(',') if p.strip()]
            params = [p for p in params if re.match(r'^\w+$', p)]
            # Detect the function body by brace counting
            depth2, started, body, j = 0, False, set(), i
            while j < n:
                for ch in lines[j]:
                    if ch == '{': depth2 += 1; started = True
                    elif ch == '}': depth2 -= 1
                if started and j != i:
                    body.add(j)
                if started and depth2 == 0:
                    funcs[func_name] = {'params': params, 'body': body}
                    i = j + 1
                    break
                j += 1
            else:
                i += 1
        else:
            i += 1
    return funcs


def get_call_site_var_values(lines, func_name, param_name, all_body_lines, lookback=6):
    """Return values of `param_name` found in lines just before `func_name(` calls
    that are NOT themselves inside any function body."""
    values = []
    for i, ln in enumerate(lines):
        if i in all_body_lines:
            continue
        if re.search(r'\b' + re.escape(func_name) + r'\s*\(', ln):
            for j in range(max(0, i - lookback), i):
                m = re.match(r'^\s*' + re.escape(param_name) + r'\s*<-\s*"([^"]*)"',
                             lines[j])
                if m and m.group(1) not in values:
                    values.append(m.group(1))
                    break
    return values


def parse_script(path):
    with open(path) as fh:
        raw = fh.readlines()
    lines = strip_comments(raw)
    var_assignments = extract_var_assignments(lines)
    info_df_paths   = extract_info_df_file_paths(lines)

    # ── function body detection ──────────────────────────────────────────────
    # Lines inside locally-defined function bodies use function parameters as
    # local variables.  Expanding those placeholders with all script-level
    # values produces false reads/writes (e.g. Translation_Table-* for every
    # concept_name, not just the ones passed at translate_table() call sites).
    # Solution: collect patterns from body lines separately and expand them
    # only with the values actually passed at each function's call sites.
    local_funcs   = extract_local_functions(lines)
    all_body_lines = set()
    for finfo in local_funcs.values():
        all_body_lines.update(finfo['body'])
    line_to_func = {li: fn for fn, fi in local_funcs.items() for li in fi['body']}

    # For each locally-defined function, collect call-site argument values
    # (values assigned to each parameter in lines just before a call).
    func_callsite_vals = {}   # {func_name: {param: [values]}}
    for fname, finfo in local_funcs.items():
        pvals = {}
        for param in finfo['params']:
            vals = get_call_site_var_values(lines, fname, param, all_body_lines)
            if vals:
                pvals[param] = vals
        if pvals:
            func_callsite_vals[fname] = pvals

    # Raw pattern accumulators: (pattern_string, func_name_or_None)
    reads_raw  = []   # (pat, func_name) – func_name is None for non-body lines
    writes_raw = []

    def match_with_idx(pattern, excl=None):
        for idx, l in enumerate(lines):
            if re.search(pattern, l):
                if not excl or not re.search(excl, l):
                    yield idx, l

    def collect_io(acc, pattern, excl=None, save_style=False):
        """Extract literal filenames and paste0 patterns from matched lines.
        Body-line patterns are tagged with their function name for restricted expansion.
        Non-body lines also get variable/df-col resolution (appended via lists).
        """
        non_body_lines = []
        for idx, l in match_with_idx(pattern, excl):
            pats = [v for v in extract_quoted(l) if is_file_like(v)]
            pats += extract_paste0_names(l)
            # sanitise: remove any accidentally embedded newlines/CRs
            pats = [p.replace('\n', '').replace('\r', '').strip() for p in pats
                    if p.replace('\n', '').replace('\r', '').strip()]
            fname = line_to_func.get(idx)  # None if not in any function body
            for p in pats:
                acc.append((p, fname))
            if fname is None:
                non_body_lines.append(l)
        # Variable / df-col resolution only for non-body lines
        for v in resolve_vars(non_body_lines, var_assignments):
            acc.append((v, None))
        for v in resolve_df_col_refs(non_body_lines, info_df_paths):
            acc.append((v, None))

    collect_io(reads_raw,  r'\bload\s*\(')
    collect_io(reads_raw,  r'\breadRDS\s*\(')
    collect_io(reads_raw,  r'\bread_rds\s*\(')
    collect_io(reads_raw,  r'\bread[_.]csv\s*\(', excl=r'gsutil|pipe\(')
    collect_io(reads_raw,  r'\bfread\s*\(')

    # save() lines get special treatment (file= keyword, not first positional arg)
    save_non_body = []
    for idx, l in match_with_idx(r'\bsave\s*\('):
        pats = [v for v in extract_quoted(l) if is_file_like(v)]
        pats += extract_paste0_names(l)
        pats = [p.replace('\n', '').replace('\r', '').strip() for p in pats
                if p.replace('\n', '').replace('\r', '').strip()]
        fname = line_to_func.get(idx)
        for p in pats:
            writes_raw.append((p, fname))
        if fname is None:
            save_non_body.append(l)
    for v in resolve_vars(save_non_body, var_assignments):
        writes_raw.append((v, None))
    for v in resolve_df_col_refs(save_non_body, info_df_paths):
        writes_raw.append((v, None))

    collect_io(writes_raw, r'\bsaveRDS\s*\(')
    collect_io(writes_raw, r'\bwrite_rds\s*\(')
    collect_io(writes_raw, r'\bwrite[_.]csv\s*\(')
    collect_io(writes_raw, r'\bwrite\.table\s*\(')
    collect_io(writes_raw, r'\bfwrite\s*\(')
    collect_io(writes_raw, r'\bggsave\s*\(')
    collect_io(writes_raw, r'\b(?:png|pdf|svg|jpeg|tiff)\s*\(')
    # download_nonexist_data(path_var, bucket, sql, type) creates the file at
    # path_var; treat it as a write so the file is not misclassified as a read.
    collect_io(writes_raw, r'\bdownload_nonexist_data\s*\(')

    # ── placeholder expansion ────────────────────────────────────────────────
    str_vars   = extract_string_var_values(lines)
    loop_ints  = extract_for_loop_int_ranges(lines)
    df_cols    = extract_df_column_values(lines)
    loop_vars  = extract_loop_var_values(lines, df_cols)

    all_var_values = {}
    for d in (str_vars, loop_ints, loop_vars):
        for k, v in d.items():
            all_var_values.setdefault(k, [])
            for val in v:
                if val not in all_var_values[k]:
                    all_var_values[k].append(val)

    # Resolve paste0-constructed variable assignments (e.g. var_name_timeline <- paste0(this_drug, "_timeline"))
    paste0_vars = resolve_paste0_var_values(lines, all_var_values)
    for k, v in paste0_vars.items():
        all_var_values.setdefault(k, [])
        for val in v:
            if val not in all_var_values[k]:
                all_var_values[k].append(val)

    adj_pairs = _find_all_adjacent_pairs(lines)

    def expand_tagged(tagged_list):
        """Expand a list of (pattern, func_name) using appropriate var values.
        Patterns from function bodies use call-site-restricted values for the
        function's own parameters; all other variables use script-level values.
        """
        result = []
        for pat, fname in tagged_list:
            if fname and fname in func_callsite_vals:
                # Build a var_values dict with call-site overrides for params
                cs_overrides = func_callsite_vals[fname]
                restricted   = dict(all_var_values)
                restricted.update(cs_overrides)
                result.extend(expand_placeholders([pat], restricted, adj_pairs))
            else:
                result.extend(expand_placeholders([pat], all_var_values, adj_pairs))
        return result

    reads  = sorted(set(expand_tagged(reads_raw)))
    writes = sorted(set(expand_tagged(writes_raw)))

    # Exclude self-referential reads: files this script creates itself should not
    # appear as dependencies on other scripts.  Two cases:
    #   1. Guarded by !file.exists (or file.exists & !overwrite): explicitly detected.
    #   2. Any file that appears in BOTH reads and writes of this script: it was
    #      written by the script itself before being read back (e.g. prep_save_*
    #      followed by translate_table in get_All_Concepts.R).
    raw_guarded = extract_not_file_exists_files(lines, var_assignments)
    guarded_expanded = set(expand_placeholders(list(raw_guarded), all_var_values, adj_pairs))
    self_created = guarded_expanded | (set(reads) & set(writes))
    reads = sorted(f for f in reads if f not in self_created)

    return reads, writes


# ── main ─────────────────────────────────────────────────────────────────────

r_scripts = sorted(
    f for f in os.listdir('.')
    if f.endswith('.R') and f not in (THIS_SCRIPT, CSV_SCRIPT)
)

rows = {}
for script in r_scripts:
    r, w = parse_script(script)
    rows[script] = {'reads': r, 'writes': w}

writes_index = {}
for script, info in rows.items():
    for f in info['writes']:
        writes_index.setdefault(f, []).append(script)

def semicolon(lst):
    return '; '.join(lst)

with open('pipeline_map.csv', 'w', newline='') as fh:
    writer = csv.writer(fh)
    writer.writerow(['script', 'files_read', 'files_created', 'dependencies'])
    for script in r_scripts:
        r = rows[script]['reads']
        w = rows[script]['writes']
        deps = sorted(set(
            src
            for f in r
            for src in writes_index.get(f, [])
            if src != script
        ))
        writer.writerow([script, semicolon(r), semicolon(w), semicolon(deps)])

print(f"Written pipeline_map.csv  ({len(r_scripts)} scripts)")

# ── topological sort → execution order ───────────────────────────────────────

def topo_sort(scripts, dep_map):
    """DFS post-order topological sort. Skips back-edges to handle cycles."""
    visited = set()
    in_stack = set()
    result = []

    def dfs(node):
        if node in in_stack:   # back-edge → skip to break cycle
            return
        if node in visited:
            return
        in_stack.add(node)
        for dep in sorted(dep_map.get(node, [])):
            dfs(dep)
        in_stack.discard(node)
        visited.add(node)
        result.append(node)

    for script in sorted(scripts):
        dfs(script)

    return result

dep_map = {}
for script in r_scripts:
    dep_map[script] = sorted(set(
        src
        for f in rows[script]['reads']
        for src in writes_index.get(f, [])
        if src != script
    ))

ordered = topo_sort(r_scripts, dep_map)

# ── write file_order.csv ──────────────────────────────────────────────────────
with open('file_order.csv', 'w', newline='') as fh:
    w = csv.writer(fh)
    w.writerow(['order', 'script'])
    for i, script in enumerate(ordered, 1):
        w.writerow([i, script])
print("Written file_order.csv")

# ── write file_order.html ─────────────────────────────────────────────────────
rows_html = ''.join(
    f'<tr><td class="num">{i}</td><td class="script">{script}</td></tr>'
    for i, script in enumerate(ordered, 1)
)

html = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Script Execution Order</title>
<style>
  body  {{ font-family: monospace; background:#1e1e2e; color:#cdd6f4; margin:2rem; }}
  h1   {{ color:#cba6f7; margin-bottom:.25rem; }}
  p.sub {{ color:#6c7086; margin-top:0; }}
  table {{ border-collapse:collapse; }}
  th  {{ background:#313244; color:#a6adc8; text-align:left;
          padding:.5rem 1.5rem .5rem .75rem; border-bottom:2px solid #45475a; }}
  td  {{ padding:.35rem .75rem; border-bottom:1px solid #313244; }}
  tr:hover td {{ background:#313244; }}
  td.num    {{ color:#6c7086; text-align:right; padding-right:1.5rem; }}
  td.script {{ color:#89dceb; }}
</style>
</head>
<body>
<h1>Script Execution Order</h1>
<p class="sub">Generated by generate_pipeline_map.py &nbsp;·&nbsp; {len(ordered)} scripts</p>
<table>
<thead><tr><th>#</th><th>Script</th></tr></thead>
<tbody>{rows_html}</tbody>
</table>
</body>
</html>
"""

with open('file_order.html', 'w') as fh:
    fh.write(html)
print("Written file_order.html")

# ── self-checks ───────────────────────────────────────────────────────────────
assert not is_file_like('.csv')
assert not is_file_like('.rds')
assert is_file_like('foo.csv')
assert not is_file_like('_index_info.rds'),  "suffix fragment should be rejected"
assert is_file_like('{this_drug}_index_info.rds'), "placeholder pattern should be accepted"
assert reconstruct_paste0('"PS_Covariates-", this_drug, ".csv"') == 'PS_Covariates-{this_drug}.csv'
assert reconstruct_paste0('"combined_table_p2_j", 1, ".csv"')    == 'combined_table_p2_j1.csv'
assert reconstruct_paste0('var_name, ".rds"')                    == '{var_name}.rds'

with open('pipeline_map.csv') as fh:
    reader = list(csv.DictReader(fh))

bare = []
for row in reader:
    for col in ('files_read', 'files_created'):
        for tok in row[col].split(';'):
            tok = tok.strip()
            if tok and tok.startswith('.') and tok[1:].isalpha():
                bare.append((row['script'], col, tok))

if bare:
    print("REMAINING BARE EXTENSIONS:")
    for script, col, tok in bare:
        print(f"  {script}: {col}={tok!r}")
    raise SystemExit(1)
else:
    print("OK: no bare extensions found")

# Spot-checks for info-df $file_path pattern
_test_info_df = extract_info_df_file_paths([
    'drug_matchedDS_info <- c("Insulins", "PS_Matched_Dataset-Insulins.rds",',
    '                         "Metformin", "PS_Matched_Dataset-Metformin.rds"',
    '                         ) %>%',
    'matrix(ncol = 2, byrow = TRUE) %>%',
    'as.data.frame() %>% setNames(c("drug", "file_path"))',
])
assert 'drug_matchedDS_info' in _test_info_df, "info_df extraction failed"
assert 'PS_Matched_Dataset-Insulins.rds'  in _test_info_df['drug_matchedDS_info']
assert 'PS_Matched_Dataset-Metformin.rds' in _test_info_df['drug_matchedDS_info']
print("OK: extract_info_df_file_paths works")

# Spot-checks for CSV content
checks = {
    # Propensity scripts: {this_drug} should now be resolved to the literal drug name
    'analysis_Propensity_Scoring_SemaglutideVsDPP4i.R': {
        'files_created': {'PS_Covariates-DPP4i.csv',
                          'PS_Matched_Dataset-DPP4i.rds',
                          'PS_Weighted_Dataset-DPP4i.rds'},
        'NOT_files_created': {'PS_Covariates-{this_drug}.csv'},
    },
    'analysis_Propensity_Scoring_SemaglutideVsInsulins.R': {
        'files_created': {'PS_Covariates-Insulins.csv',
                          'PS_Matched_Dataset-Insulins.rds'},
    },
    # Survey answers: j in 1:3 should be expanded
    'get_Participant_survey_answers_part2.R': {
        'files_created': {'participant_survey_answers_p2.csv',
                          'participant_survey_answers_p2_collapsed.csv',
                          'this_mapping_start.csv',
                          'combined_table_p2_j1.csv',
                          'combined_table_p2_j2.csv',
                          'combined_table_p2_j3.csv'},
    },
    # info-df $file_path reads
    'analysis_Negative_Binomial_Regression.R': {
        'files_read': {'PS_Matched_Dataset-Insulins.rds',
                       'PS_Matched_Dataset-Metformin.rds',
                       'PS_Matched_Dataset-DPP4i.rds'},
    },
    'get_Antidiabetic_Drug_Exposure.R': {
        'files_read': {'Data_Prepped/IDs-Semaglutide-Drug_Exposure.txt',
                       'Data_Prepped/IDs-Insulins-Drug_Exposure.txt'},
    },
    # diagnoses_timeline_info reads
    'get_Diagnosis_Timeline.R': {
        'files_read': {'Data_Prepped/Prepped_Data-All_Participants-Type_2_Diabetes_Mellitus-Condition_Occurrence.rds',
                       'Data_Prepped/Prepped_Data-All_Participants-Obesity-Combined.rds'},
    },
    # concept_name / table_type pairs in get_All_Concepts.R
    'get_All_Concepts.R': {
        'files_created': {
            'Data_Prepped/Prepped_Data-All_Participants-Type_2_Diabetes_Mellitus-Condition_Occurrence.rds',
            'Data_Prepped/Prepped_Data-All_Participants-Semaglutide-Drug_Exposure.rds',
            'Data_Prepped/Unique_Concept_Detail-All_Participants-Antipsychotics-Drug_Exposure.csv',
        },
        # Prepped_Data files are self-created (written then read back), AND
        # Issue 1: Translation_Table reads must be restricted to concepts actually
        # passed to translate_table() – not all concept_names in the script.
        'NOT_files_read': {
            'Data_Prepped/Prepped_Data-All_Participants-Type_2_Diabetes_Mellitus-Condition_Occurrence.rds',
            'Data_Prepped/Prepped_Data-All_Participants-Semaglutide-Drug_Exposure.rds',
            'Data_Prepped/Translation_Table-Type_2_Diabetes_Mellitus.csv',
            'Data_Prepped/Translation_Table-Depression.csv',
            'Data_Prepped/Translation_Table-Hypertension.csv',
            'Data_Prepped/Translation_Table-Persons_with_Potential_Health_Hazards_Related_to_Socioeconomic_and_Psychosocial_Circumstances.csv',
        },
        # Issue 1/2: translate_table() is never called for non-drug concept_names,
        # so Unique_Concept_Detail entries for those must not appear in files_created.
        'NOT_files_created': {
            'Data_Prepped/Unique_Concept_Detail-All_Participants-Type_2_Diabetes_Mellitus-Condition_Occurrence.csv',
            'Data_Prepped/Unique_Concept_Detail-All_Participants-Persons_with_Potential_Health_Hazards_Related_to_Socioeconomic_and_Psychosocial_Circumstances-Observation.csv',
        },
    },
    # Issue 3: both consecutive-instance and consecutive-period .rds files must
    # appear in files_created for get_Antidepressant_Treatment_Timeline_v2.R.
    'get_Antidepressant_Treatment_Timeline_v2.R': {
        'files_created': {
            'antidepressant_antipsychotic_consecutive_instance.rds',
            'antidepressant_antipsychotic_consecutive_period.rds',
        },
    },
    # Issue 4: download_nonexist_data() creates files – they must appear in
    # files_created, NOT in files_read.
    'get_zip.R': {
        'files_created': {'obs_table.csv', 'zip_table.csv',
                          'zip_table_full.csv', 'person_ext_table.csv'},
        'NOT_files_read': {'obs_table.csv', 'zip_table.csv',
                           'zip_table_full.csv', 'person_ext_table.csv'},
    },
    # explore_Visit_Timeline.R: {this_drug}_index_info.rds files are self-created
    # (written when new, loaded when existing via file.exists & !overwrite guard).
    # They must appear in files_created but NOT in files_read.
    'explore_Visit_Timeline.R': {
        'files_created': {'Semaglutide_index_info.rds',
                          'Insulins_index_info.rds',
                          'DPP4i_index_info.rds'},
        'NOT_files_read': {'Semaglutide_index_info.rds',
                           'Insulins_index_info.rds',
                           'DPP4i_index_info.rds',
                           '_index_info.rds'},
    },
    # Antidiabetic timelines: var_name_timeline should be expanded
    'get_Antidiabetic_Timelines_v2.R': {
        'files_created': {'Semaglutide_timeline.rds',
                          'Insulins_timeline.rds',
                          'Semaglutide_first_drug_record.rds'},
    },
    # explore_PS_Covariate_summary reads PS_Covariates per drug
    'explore_PS_Covariate_summary.R': {
        'files_read': {'PS_Covariates-DPP4i.csv',
                       'PS_Covariates-Insulins.csv'},
    },
}
index = {r['script']: r for r in reader}
for script, cols in checks.items():
    for col, expected in cols.items():
        if col.startswith('NOT_'):
            real_col = col[4:]
            actual = set(t.strip() for t in index[script][real_col].split(';') if t.strip())
            bad = expected & actual
            if bad:
                print(f"STILL PRESENT (should be gone) in {script} {real_col}: {bad}")
            else:
                print(f"OK  {script}: {real_col} correctly absent: {expected}")
        else:
            actual = set(t.strip() for t in index[script][col].split(';') if t.strip())
            missing = expected - actual
            if missing:
                print(f"MISSING in {script} {col}: {missing}")
            else:
                print(f"OK  {script}: {col} contains expected values")
