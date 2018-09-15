[[src]](https://github.com/ghc/ghc/tree/master/compiler/vectorise/Vectorise/Generic/PData.hs)

mk_fam_inst :: TyCon -> TyCon -> (TyCon, [Type])
mk_fam_inst fam_tc arg_tc
  = (fam_tc, [mkTyConApp arg_tc . mkTyVarTys $ tyConTyVars arg_tc])
