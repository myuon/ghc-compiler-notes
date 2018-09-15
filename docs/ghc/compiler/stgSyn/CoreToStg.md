[[src]](https://github.com/ghc/ghc/tree/master/compiler/stgSyn/CoreToStg.hs)

    SDM: disabled.  Eval/Apply can't handle functions with arity zero very
    well; and making these into simple non-updatable thunks breaks other
    assumptions (namely that they will be entered only once).

    upd_flag | isPAP env rhs  = ReEntrant
             | otherwise      = Updatable

-- Detect thunks which will reduce immediately to PAPs, and make them
-- non-updatable.  This has several advantages:
--
--         - the non-updatable thunk behaves exactly like the PAP,
--
--         - the thunk is more efficient to enter, because it is
--           specialised to the task.
--
--         - we save one update frame, one stg_update_PAP, one update
--           and lots of PAP_enters.
--
--         - in the case where the thunk is top-level, we save building
--           a black hole and furthermore the thunk isn't considered to
--           be a CAF any more, so it doesn't appear in any SRTs.
--
-- We do it here, because the arity information is accurate, and we need
-- to do it before the SRT pass to save the SRT entries associated with
-- any top-level PAPs.

isPAP env (StgApp f args) = listLengthCmp args arity == LT -- idArity f > length args
                              where
                                 arity = stgArity f (lookupBinding env f)
isPAP env _               = False



 ToDo:
          upd = if isOnceDem dem
                    then (if isNotTop toplev
                            then SingleEntry    -- HA!  Paydirt for "dem"
                            else
                     (if debugIsOn then trace "WARNING: SE CAFs unsupported, forcing UPD instead" else id) $
                     Updatable)
                else Updatable
        -- For now we forbid SingleEntry CAFs; they tickle the
        -- ASSERT in rts/Storage.c line 215 at newCAF() re mut_link,
        -- and I don't understand why.  There's only one SE_CAF (well,
        -- only one that tickled a great gaping bug in an earlier attempt
        -- at ClosureInfo.getEntryConvention) in the whole of nofib,
        -- specifically Main.lvl6 in spectral/cryptarithm2.
        -- So no great loss.  KSW 2000-07.
