@AbapCatalog.sqlViewName: 'ZPL_ORD_PXQ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PL: Ordenes PxQ'
define view ZPL_COORDER_PXQ as select from /bi0/mcoorder as a
{
    key right( a.coorder, 10 ) as ref_doc,
    key a.coorder,
    a./bic/zmar
}
where ( a./bic/zmar = 'DIS' or a./bic/zmar = 'FLE' or
        a./bic/zmar = 'HOM' or a./bic/zmar = 'INE' or
        a./bic/zmar = 'INV' or a./bic/zmar = 'REC' or a./bic/zmar =  'VAC' )
