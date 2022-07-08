struct Symb
    symb::Symbol
    inverse::Bool
end

Symb(x::Symbol) = Symb(x, false)
Symb(x::Expr) = Symb(x.args[2], true)

Base.inv(symb::Symb) = Symb(symb.symb, !symb.inverse)

isinv(symb1, symb2) =
    symb1.symb == symb2.symb && symb1.inverse != symb2.inverse

Base.show(io::IO, symb::Symb) =
    print(io, symb.symb, symb.inverse ? "-1" : "")

struct Word
    w::Vector{Symb}

    Word(w) = new(reduce(w))
end

Base.:(==)(word1::Word, word2::Word) = word1.w == word2.w

Word(symb::Symb) = Word([symb])

Base.one(::Union{Type{Word}, Word}) = Word([])

Base.show(io::IO, word::Word) =
    foreach(letter -> print(io, letter, " "), word.w)

Base.length(word::Word) = length(word.w)
Base.getindex(word::Word, idx::Integer) = getindex(word.w, idx)
Base.getindex(word::Word, idx::AbstractRange) = Word(getindex(word.w, idx))
Base.firstindex(::Word) = 1
Base.lastindex(word::Word) = length(word)

function Base.inv(word::Word)
    ret_vec = map(inv, word.w)

    Word(reverse!(ret_vec))
end

function Base.append!(word1::Word, word2::Word)
    append!(word1.w, word2.w)

    word1
end

concat(word1, word2) = Word(vcat(word1.w, word2.w))

for fun ∈ [:push!, :pushfirst!]
    @eval function Base.$fun(word::Word, symb)
        $fun(word.w, symb)

        word
    end
end

function reduce_1step(arr::AbstractVector)
    length(arr) < 2 &&
        return arr
    isinv(arr[1], arr[2]) &&
        return reduce_1step(arr[3:end])
    pushfirst!(reduce_1step(arr[2:end]), arr[1])
end

function reduce(arr::AbstractVector)
    old_arr = arr
    ret = reduce_1step(arr)
    while length(ret) != length(old_arr)
        old_arr = deepcopy(ret)
        ret = reduce_1step(ret)
    end

    ret
end

macro word(args...)
    Word([map(Symb, args)...])
end

import JCheck: generate

function generate(rng, ::Type{Word}, n)
    lens = rand(rng, 1:100, n)

    symbols_vec = map(lens) do len
        Symbol.(rand(rng, 'a':'c', len))
    end
    inverses_vec = map(len -> rand(rng, Bool, len), lens)

    map(symbols_vec, inverses_vec) do symbols, inverses
        Word(map(Symb, symbols, inverses))
    end
end

using JCheck: Quickcheck

qc = Quickcheck("Free Group Over {a, b, c}")

using JCheck: @add_predicate

pred_inverse(word) =
    concat(word, inv(word)) ==
    concat(inv(word), word) ==
    one(Word)

@add_predicate(qc,
               "Inverse",
               word::Word -> pred_inverse(word))

pred_commute(w1, w2) = concat(w1, w2) == concat(w2, w1)

@add_predicate(qc,
               "Commutativity",
               (word::Word, word2::Word) ->
                   pred_commute(word, word2))

import JCheck: specialcases

specialcases(::Type{Word}) = [one(Word)]

import JCheck: shrink, shrinkable

shrinkable(word::Word) = length(word) > 2

function shrink(word::Word)
    shrinkable(word) || return [word]

    n = length(word) ÷ 2
    [word[1:n], word[(n+1):end]]
end

using Test: @test, @testset

using JCheck: @quickcheck

@testset "Group Properties" begin
    @test (@word a b) == (@word a b b-1 b)

    @quickcheck qc
end
