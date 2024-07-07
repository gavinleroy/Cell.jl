module Cell

using MacroTools: postwalk, @capture
using ReusePatterns

abstract type BrownCell end

val(c::BrownCell) = c.val
assigns(c::BrownCell) = c.assigns

Base.show(io::IO, c::BrownCell) =
    print(io, c.val)


diff(_lhs::BrownCell, _rhs::BrownCell) =
    error("Cannot diff cells of different types")


macro cell(f)
    f.head == :function || error("Not a function expression.")
    body = f.args[2]
    funcname = f.args[1].args[1]
    funcargs = f.args[1].args[2:end]

    struct_name = Meta.gensym(funcname)
    local def = quote
        struct $struct_name <: BrownCell
            val::Any
            assigns::Dict{Symbol,Any}
        end
        function diff(lhs::$struct_name, rhs::$struct_name)
            println(lhs.val, " vs ", rhs.val)
            for (key, lhs_value) in lhs.assigns
                rhs_value = rhs.assigns[key]
                println(key, "= ", lhs_value, " vs ", rhs_value)
            end
        end
    end
    eval(def)

    return quote
        function $(esc(funcname))($(map(esc, funcargs)...))
            local function inner(assigns)
                $(postwalk(x -> @capture(x, s_ = v_) ?
                                quote
                        local vv = $v
                        $s = vv
                        assigns[$(Expr(:quote, s))] = vv
                    end
                                : x, body))

            end
            local assigns = Dict{Symbol,Any}()
            local ret = inner(assigns)
            $struct_name(ret, assigns)
        end
    end
end

@cell function hyp(x, y)
    i = x * x
    j = y * y
    sqrt(i + j)
end

@cell function sq(x)
    x * x
end

h1 = hyp(3, 4)
h2 = hyp(5, 12)

end # module Cell
