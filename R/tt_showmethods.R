
treestruct <- function(obj, ind = 0L) {
    nc = ncol(obj)
    cat(rep(" ", times = ind),
        sprintf("[%s] %s", class(obj), obj_name(obj)),
        sep = "")
    if(!is(obj, "ElementaryTable") && nrow(obj@content) > 0 ){
        crows = nrow(content_table(obj))
        ccols = if(crows == 0) 0 else nc
        cat(sprintf(" [cont: %d x %d]",
                      crows, ccols))
    }
    if(is(obj, "VTableTree") && length(tree_children(obj))) {
        kids = tree_children(obj)
        if(are(kids, "TableRow")) {
            cat(sprintf( " (%d x %d)\n",
                        length(kids), nc))
        } else {
            cat("\n")
            lapply(kids, treestruct, ind = ind+1)
        }
    }
    invisible(NULL)
}




docat = function(obj) {
    if(!is(obj, "ElementaryTable") && nrow(obj@content) > 0 ){
        crows = nrow(obj@content)
        ccols = if(crows == 0) 0 else ncol(obj@content)
        cat(rep("*", obj@level), sprintf(" %s [%d x %d]\n",
                                         obj_label(content_table(obj)),
                                         crows, ccols),
            sep = "")

    }
    if(is(obj, "VTableTree") && length(tree_children(obj))) {
        kids = tree_children(obj)
        isr = which(sapply(kids, is, "TableRow"))
        ## can they ever be inteerspersed, I don't think so
        if(length(isr)) {
            r = kids[[isr[1]]]
            lv = r@level
            if(is.na(lv)) lv = 0
            cat(rep("*", lv),
                sprintf(" %s [%d x %d] \n",
                        obj_label(obj),
                        length(kids),
                        length(row_values(r))),
                sep="")
            kids = kids[-isr]
        }
        lapply(kids, docat)
    }
    invisible(NULL)
}

ploads_to_str = function(x, collapse = ":") {
    if(is(x, "Split")) {
        sapply(spl_payload(x), ploads_to_str)
    } else if (is.list(x) && are(x, "list")) {
        paste0(c("(", paste(sapply(x, ploads_to_str), collapse = ", "), ")"))
    } else if(is.list(x) && are(x, "Split")) {
        ploads_to_str(lapply(x, spl_payload))
    } else {
        sapply(x,
               paste,
               collapse = collapse)
    }
}


setGeneric("payloadmsg", function(spl) standardGeneric("payloadmsg"))

setMethod("payloadmsg", "VarLevelSplit",
          function(spl) {
    spl_payload(spl)
})

setMethod("payloadmsg", "MultiVarSplit",
          function(spl) "var")

setMethod("payloadmsg", "VarLevWBaselineSplit",
          function(spl) paste0(spl_payload(spl), "[bsl ",
                               spl@ref_group_value, # XXX XXX
                               "]"))

setMethod("payloadmsg", "ManualSplit",
          function(spl) "mnl")


setMethod("payloadmsg", "ANY",
          function(spl) {
    warning("don't nkow how to make payload print message for Split of class", class(spl))
    "XXX"
})

spldesc = function(spl, value = "") {
    value = rawvalues(value)
    payloadmsg = payloadmsg(spl)
    format = "%s (%s)"
    sprintf(format,
            value,
            payloadmsg)
}


layoutmsg = function(obj) {
    ## if(!is(obj, "VLayoutNode"))
    ##     stop("how did a non layoutnode object get in docatlayout??")

    pos = tree_pos(obj)
    spllst = pos_splits(pos)
    spvallst = pos_splvals(pos)
    if(istree <- is(obj, "LayoutAxisTree")) {
        kids = tree_children(obj)
        return(unlist(lapply(kids, layoutmsg)))

    }

    msg = paste(collapse = " -> ",
                mapply(spldesc,
                       spl = spllst,
                       value = spvallst))
    msg
}

setMethod("show", "LayoutAxisTree",
          function(object) {
    msg = layoutmsg(object)
    cat(msg, "\n")
    invisible(object)
})


setGeneric("spltype_abbrev", function(obj) standardGeneric("spltype_abbrev"))

setMethod("spltype_abbrev", "VarLevelSplit",
          function(obj) "lvls")

setMethod("spltype_abbrev", "VarLevWBaselineSplit",
          function(obj) paste("ref_group", obj@ref_group_value))


setMethod("spltype_abbrev", "MultiVarSplit",
          function(obj) "vars")

setMethod("spltype_abbrev", "VarStaticCutSplit",
          function(obj) "scut")

setMethod("spltype_abbrev", "VarDynCutSplit",
          function(obj) "dcut")
setMethod("spltype_abbrev", "AllSplit",
          function(obj) "all obs")
setMethod("spltype_abbrev", "NULLSplit",
          function(obj) "no obs")

setMethod("spltype_abbrev", "AnalyzeVarSplit",
          function(obj) "** analysis **")

setMethod("spltype_abbrev", "CompoundSplit",
          function(obj) paste("compound", paste(sapply(spl_payload(obj), spltype_abbrev), collapse = " ")))

setMethod("spltype_abbrev", "AnalyzeMultiVars",
          function(obj) "** multivar analysis **")
setMethod("spltype_abbrev", "AnalyzeColVarSplit",
          function(obj) "** col-var analysis **")




docat_splitvec = function(object, indent = 0) {
    if(indent > 0)
        cat(rep(" ", times = indent), sep = "")
    if(length(object) == 1L && is(object[[1]], "VTableNodeInfo")) {
        tab = object[[1]]
        msg = sprintf("A Pre-Existing Table [%d x %d]",
                      nrow(tab), ncol(tab))
    } else {

        plds = ploads_to_str(lapply(object, spl_payload))

        tabbrev = sapply(object, spltype_abbrev)
        msg = paste(collapse = " -> ",
                    paste0(plds, " (", tabbrev, ")"))
    }
    cat(msg, "\n")
}

setMethod("show", "SplitVector",
          function(object) {

    cat("A SplitVector Pre-defining a Tree Structure\n\n")
    docat_splitvec(object)
    cat("\n")
    invisible(object)
})


docat_predataxis = function(object, indent = 0) {
    lapply(object, docat_splitvec)
}

setMethod("show", "PreDataColLayout",
          function(object) {
    cat("A Pre-data Column Layout Object\n\n")
    docat_predataxis(object)
    invisible(object)
})


setMethod("show", "PreDataRowLayout",
          function(object) {
    cat("A Pre-data Row Layout Object\n\n")
    docat_predataxis(object)
    invisible(object)
})


setMethod("show", "PreDataTableLayouts",
          function(object) {
    cat("A Pre-data Table Layout\n")
    cat("\nColumn-Split Structure:\n")
    docat_predataxis(object@col_layout)
    cat("\nRow-Split Structure:\n")
    docat_predataxis(object@row_layout)
    cat("\n")
    invisible(object)
})


setMethod("show", "TreePos",
          function(object) {
    chars = mapply(function(label, val)
        {
            paste0(label, " [", val, "]")
        }, label = pos_split_labels(object),
        val = pos_splval_labels(object))

    msg = paste(chars, collapse = " -> ")
    cat("An object of class ", class(object), "\n\n", msg)
    invisible(object)
})


setMethod("show", "InstantiatedColumnInfo",
          function(object) {
    layoutmsg = layoutmsg( coltree(object))
    cat("An InstantiatedColumnInfo object",
        "Columns:",
        layoutmsg,
        if(disp_ccounts(object))
            paste("ColumnCounts:\n",
                  paste(col_counts(object),
                        collapse = ", ")),
        "",
        sep = "\n")
    invisible(object)
})


#' @rdname int_methods
setMethod("print", "VTableTree", function(x, ...) {
    msg <- toString(x, ...)
    cat(msg)
    invisible(x)
})

#' @rdname int_methods
setMethod("show", "VTableTree", function(object) {
    cat(toString(object))
    invisible(object)
})


setMethod("show", "TableRow", function(object) {
    cat(sprintf("[%s indent_mod %d]: %s   %s\n",
                class(object),
                indent_mod(object),
                obj_label(object),
                paste(as.vector(get_formatted_cells(object)),
                      collapse = "   ")))
    invisible(object)
})