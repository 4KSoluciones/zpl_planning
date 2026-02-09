@AbapCatalog.sqlViewName: 'ZPL_CUENTAS_1000'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Cuentas por Usuario Resp'
define view ZPL_CUENTA_1000 
with parameters i_user : username
as select from /bi0/mgl_account as a 
{
key a./bic/ztec_cuen, 
          //a~/bic/ztec_cue2, 
key a.gl_account
   
}
where ( a./bic/ztec_cuen = $parameters.i_user or a./bic/ztec_cuen = '' )
and a.chrt_accts = 'SQS'
//AND a.gl_account IN @gr_cuenta
and a./bic/znivel1 = '07'
and ( a./bic/zcla_cue = 'P' and a./bic/zcla_cue = 'S' )
and a.objvers = 'A'

union


select from /bi0/mgl_account as a 
{
key a./bic/ztec_cuen, 
          //a~/bic/ztec_cue2, 
key a.gl_account
   
}
where ( a./bic/ztec_cue2 = $parameters.i_user or a./bic/ztec_cue2 = '' )
and a.chrt_accts = 'SQS'

and a./bic/znivel1 = '07'
and ( a./bic/zcla_cue = 'P' and a./bic/zcla_cue = 'S' )
and a.objvers = 'A'
