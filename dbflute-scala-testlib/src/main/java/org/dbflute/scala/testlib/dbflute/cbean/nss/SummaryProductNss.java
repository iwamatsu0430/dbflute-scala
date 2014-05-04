package org.dbflute.scala.testlib.dbflute.cbean.nss;

import org.dbflute.scala.testlib.dbflute.cbean.cq.SummaryProductCQ;
import org.seasar.dbflute.cbean.ConditionQuery;

/**
 * The nest select set-upper of SUMMARY_PRODUCT.
 * @author DBFlute(AutoGenerator)
 */
public class SummaryProductNss {

    // ===================================================================================
    //                                                                           Attribute
    //                                                                           =========
    protected SummaryProductCQ _query;
    public SummaryProductNss(SummaryProductCQ query) { _query = query; }
    public boolean hasConditionQuery() { return _query != null; }

    // ===================================================================================
    //                                                                     Nested Relation
    //                                                                     ===============
    /**
     * With nested relation columns to select clause. <br />
     * (商品ステータス)PRODUCT_STATUS by my PRODUCT_STATUS_CODE, named 'productStatus'.
     * @return The set-upper of more nested relation. {...with[nested-relation].with[more-nested-relation]} (NotNull)
     */
    public ProductStatusNss withProductStatus() {
        _query.doNss(new SummaryProductCQ.NssCall() { public ConditionQuery qf() { return _query.queryProductStatus(); }});
        return new ProductStatusNss(_query.queryProductStatus());
    }

}
