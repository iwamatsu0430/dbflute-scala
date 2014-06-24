package com.example.dbflute.scala.dbflute.cbean.cq.bs;

import scala.collection.immutable._;
import scala.collection.JavaConverters._;

import java.util.Collection;
import java.util.Date;
import java.sql.Timestamp;

import org.seasar.dbflute.cbean._;
import org.seasar.dbflute.cbean.AbstractConditionQuery._;
import org.seasar.dbflute.cbean.chelper._;
import org.seasar.dbflute.cbean.ckey._;
import org.seasar.dbflute.cbean.coption._;
import org.seasar.dbflute.cbean.cvalue.ConditionValue;
import org.seasar.dbflute.cbean.sqlclause.SqlClause;
import org.seasar.dbflute.dbmeta.DBMetaProvider;
import com.example.dbflute.scala.dbflute.allcommon._;
import com.example.dbflute.scala.dbflute.cbean._;
import com.example.dbflute.scala.dbflute.cbean.cq._;

/**
 * The abstract condition-query of PRODUCT.
 * @author DBFlute(AutoGenerator)
 */
abstract class AbstractBsProductCQ(referrerQuery: ConditionQuery, sqlClause: SqlClause, aliasName: String, nestLevel: Integer)
        extends AbstractConditionQuery(referrerQuery, sqlClause, aliasName, nestLevel) {

    // ===================================================================================
    //                                                                     DBMeta Provider
    //                                                                     ===============
    @Override
    protected def xgetDBMetaProvider(): DBMetaProvider = {
        return DBMetaInstanceHandler.getProvider();
    }

    // ===================================================================================
    //                                                                          Table Name
    //                                                                          ==========
    def getTableDbName(): String = {
        return "PRODUCT";
    }

    // ===================================================================================
    //                                                                               Query
    //                                                                               =====
    
    /**
     * Equal(=). And NullIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_ID: {PK, ID, NotNull, INTEGER(10)}
     * @param productId The value of productId as equal. (NullAllowed: if null, no condition)
     */
     def setProductId_Equal(productId: Integer): Unit = {
        doSetProductId_Equal(productId);
    }

    protected def doSetProductId_Equal(productId: Integer): Unit = {
        regProductId(CK_EQ, productId);
    }

    /**
     * NotEqual(&lt;&gt;). And NullIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_ID: {PK, ID, NotNull, INTEGER(10)}
     * @param productId The value of productId as notEqual. (NullAllowed: if null, no condition)
     */
     def setProductId_NotEqual(productId: Integer): Unit = {
        doSetProductId_NotEqual(productId);
    }

    protected def doSetProductId_NotEqual(productId: Integer): Unit = {
        regProductId(CK_NES, productId);
    }

    /**
     * GreaterThan(&gt;). And NullIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_ID: {PK, ID, NotNull, INTEGER(10)}
     * @param productId The value of productId as greaterThan. (NullAllowed: if null, no condition)
     */
    def setProductId_GreaterThan(productId: Integer): Unit = {
        regProductId(CK_GT, productId);
    }

    /**
     * LessThan(&lt;). And NullIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_ID: {PK, ID, NotNull, INTEGER(10)}
     * @param productId The value of productId as lessThan. (NullAllowed: if null, no condition)
     */
    def setProductId_LessThan(productId: Integer): Unit = {
        regProductId(CK_LT, productId);
    }

    /**
     * GreaterEqual(&gt;=). And NullIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_ID: {PK, ID, NotNull, INTEGER(10)}
     * @param productId The value of productId as greaterEqual. (NullAllowed: if null, no condition)
     */
    def setProductId_GreaterEqual(productId: Integer): Unit = {
        regProductId(CK_GE, productId);
    }

    /**
     * LessEqual(&lt;=). And NullIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_ID: {PK, ID, NotNull, INTEGER(10)}
     * @param productId The value of productId as lessEqual. (NullAllowed: if null, no condition)
     */
    def setProductId_LessEqual(productId: Integer): Unit = {
        regProductId(CK_LE, productId);
    }

    /**
     * RangeOf with various options. (versatile) <br />
     * {(default) minNumber &lt;= column &lt;= maxNumber} <br />
     * And NullIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_ID: {PK, ID, NotNull, INTEGER(10)}
     * @param minNumber The min number of productId. (NullAllowed: if null, no from-condition)
     * @param maxNumber The max number of productId. (NullAllowed: if null, no to-condition)
     * @param optionCall The callback for option of range-of. (NotNull)
     */
    def setProductId_RangeOf(minNumber: Integer, maxNumber: Integer)(optionCall: (RangeOfOption) => Unit): Unit = {
        regROO(minNumber, maxNumber, getCValueProductId(), "PRODUCT_ID", callbackROOP(optionCall));
    }

    /**
     * InScope {in (1, 2)}. And NullIgnored, NullElementIgnored, SeveralRegistered. <br />
     * PRODUCT_ID: {PK, ID, NotNull, INTEGER(10)}
     * @param productIdList The collection of productId as inScope. (NullAllowed: if null (or empty), no condition)
     */
    def setProductId_InScope(productIdList: List[Int]): Unit = {
        doSetProductId_InScope(toMutableValueCollectionImplicitly(productIdList));
    }

    protected def doSetProductId_InScope(productIdList: Collection[Integer]): Unit = {
        regINS(CK_INS, cTL(productIdList), getCValueProductId(), "PRODUCT_ID");
    }

    /**
     * NotInScope {not in (1, 2)}. And NullIgnored, NullElementIgnored, SeveralRegistered. <br />
     * PRODUCT_ID: {PK, ID, NotNull, INTEGER(10)}
     * @param productIdList The collection of productId as notInScope. (NullAllowed: if null (or empty), no condition)
     */
    def setProductId_NotInScope(productIdList: List[Integer]): Unit = {
        doSetProductId_NotInScope(if (productIdList != null) { productIdList.map(_.asInstanceOf[Integer]).asJava } else { null });
    }

    protected def doSetProductId_NotInScope(productIdList: Collection[Integer]): Unit = {
        regINS(CK_NINS, cTL(productIdList), getCValueProductId(), "PRODUCT_ID");
    }

    /**
     * Set up ExistsReferrer (correlated sub-query). <br />
     * {exists (select PRODUCT_ID from PURCHASE where ...)} <br />
     * (購入)PURCHASE by PRODUCT_ID, named 'purchaseAsOne'.
     * <pre>
     * cb.query().<span style="color: #DD4747">existsPurchaseList</span>(new SubQuery&lt;PurchaseCB&gt;() {
     *     public void query(PurchaseCB subCB) {
     *         subCB.query().setXxx...
     *     }
     * });
     * </pre>
     * @param subQuery The sub-query of PurchaseList for 'exists'. (NotNull)
     */
    def existsPurchaseList(subQuery: (PurchaseCB) => Unit): Unit = {
        assertObjectNotNull("subQuery", subQuery);
        val cb: PurchaseCB = new PurchaseCB(); cb.xsetupForExistsReferrer(this);
        try { lock(); subQuery(cb); } finally { unlock(); }
        val pp: String = keepProductId_ExistsReferrer_PurchaseList(cb.query());
        registerExistsReferrer(cb.query(), "PRODUCT_ID", "PRODUCT_ID", pp, "purchaseList");
    }
    def keepProductId_ExistsReferrer_PurchaseList(sq: PurchaseCQ): String;

    /**
     * Set up NotExistsReferrer (correlated sub-query). <br />
     * {not exists (select PRODUCT_ID from PURCHASE where ...)} <br />
     * (購入)PURCHASE by PRODUCT_ID, named 'purchaseAsOne'.
     * <pre>
     * cb.query().<span style="color: #DD4747">notExistsPurchaseList</span>(new SubQuery&lt;PurchaseCB&gt;() {
     *     public void query(PurchaseCB subCB) {
     *         subCB.query().setXxx...
     *     }
     * });
     * </pre>
     * @param subQuery The sub-query of ProductId_NotExistsReferrer_PurchaseList for 'not exists'. (NotNull)
     */
    def notExistsPurchaseList(subQuery: (PurchaseCB) => Unit): Unit = {
        assertObjectNotNull("subQuery", subQuery);
        val cb: PurchaseCB = new PurchaseCB(); cb.xsetupForExistsReferrer(this);
        try { lock(); subQuery(cb); } finally { unlock(); }
        val pp: String = keepProductId_NotExistsReferrer_PurchaseList(cb.query());
        registerNotExistsReferrer(cb.query(), "PRODUCT_ID", "PRODUCT_ID", pp, "purchaseList");
    }
    def keepProductId_NotExistsReferrer_PurchaseList(sq: PurchaseCQ): String;

    /**
     * Set up InScopeRelation (sub-query). <br />
     * {in (select PRODUCT_ID from PURCHASE where ...)} <br />
     * (購入)PURCHASE by PRODUCT_ID, named 'purchaseAsOne'.
     * @param subQuery The sub-query of PurchaseList for 'in-scope'. (NotNull)
     */
    def inScopePurchaseList(subQuery: (PurchaseCB) => Unit): Unit = {
        assertObjectNotNull("subQuery", subQuery);
        val cb: PurchaseCB = new PurchaseCB(); cb.xsetupForInScopeRelation(this);
        try { lock(); subQuery(cb); } finally { unlock(); }
        val pp: String = keepProductId_InScopeRelation_PurchaseList(cb.query());
        registerInScopeRelation(cb.query(), "PRODUCT_ID", "PRODUCT_ID", pp, "purchaseList");
    }
    def keepProductId_InScopeRelation_PurchaseList(sq: PurchaseCQ): String;

    /**
     * Set up NotInScopeRelation (sub-query). <br />
     * {not in (select PRODUCT_ID from PURCHASE where ...)} <br />
     * (購入)PURCHASE by PRODUCT_ID, named 'purchaseAsOne'.
     * @param subQuery The sub-query of PurchaseList for 'not in-scope'. (NotNull)
     */
    def notInScopePurchaseList(subQuery: (PurchaseCB) => Unit): Unit = {
        assertObjectNotNull("subQuery", subQuery);
        val cb: PurchaseCB = new PurchaseCB(); cb.xsetupForInScopeRelation(this);
        try { lock(); subQuery(cb); } finally { unlock(); }
        val pp: String = keepProductId_NotInScopeRelation_PurchaseList(cb.query());
        registerNotInScopeRelation(cb.query(), "PRODUCT_ID", "PRODUCT_ID", pp, "purchaseList");
    }
    def keepProductId_NotInScopeRelation_PurchaseList(sq: PurchaseCQ): String;

    def xsderivePurchaseList(fn: String, sq: SubQuery[PurchaseCB], al: String, op: DerivedReferrerOption): Unit = {
        assertObjectNotNull("subQuery", sq);
        val cb: PurchaseCB = new PurchaseCB(); cb.xsetupForDerivedReferrer(this);
        try { lock(); sq.query(cb); } finally { unlock(); }
        val pp: String = keepProductId_SpecifyDerivedReferrer_PurchaseList(cb.query());
        registerSpecifyDerivedReferrer(fn, cb.query(), "PRODUCT_ID", "PRODUCT_ID", pp, "purchaseList", al, op);
    }
    def keepProductId_SpecifyDerivedReferrer_PurchaseList(sq: PurchaseCQ): String;

    /**
     * Prepare for (Query)DerivedReferrer (correlated sub-query). <br />
     * {FOO &lt;= (select max(BAR) from PURCHASE where ...)} <br />
     * (購入)PURCHASE by PRODUCT_ID, named 'purchaseAsOne'.
     * <pre>
     * cb.query().<span style="color: #DD4747">derivedPurchaseList()</span>.<span style="color: #DD4747">max</span>(new SubQuery&lt;PurchaseCB&gt;() {
     *     public void query(PurchaseCB subCB) {
     *         subCB.specify().<span style="color: #DD4747">columnFoo...</span> <span style="color: #3F7E5E">// derived column by function</span>
     *         subCB.query().setBar... <span style="color: #3F7E5E">// referrer condition</span>
     *     }
     * }).<span style="color: #DD4747">greaterEqual</span>(123); <span style="color: #3F7E5E">// condition to derived column</span>
     * </pre>
     * @return The object to set up a function for referrer table. (NotNull)
     */
    def derivedPurchaseList(): ScrHpQDRFunction[PurchaseCB] = {
        return toScalaQDRFunction(xcreateQDRFunctionPurchaseList());
    }
    protected def xcreateQDRFunctionPurchaseList(): HpQDRFunction[PurchaseCB] = {
        return new HpQDRFunction[PurchaseCB](new HpQDRSetupper[PurchaseCB]() {
            def setup(fn: String, sq: SubQuery[PurchaseCB], rd: String, vl: Object, op: DerivedReferrerOption): Unit = {
                xqderivePurchaseList(fn, sq, rd, vl, op);
            }
        });
    }
    def xqderivePurchaseList(fn: String, sq: SubQuery[PurchaseCB], rd: String, vl: Object, op: DerivedReferrerOption): Unit = {
        assertObjectNotNull("subQuery", sq);
        val cb: PurchaseCB = new PurchaseCB(); cb.xsetupForDerivedReferrer(this);
        try { lock(); sq.query(cb); } finally { unlock(); }
        val sqpp: String = keepProductId_QueryDerivedReferrer_PurchaseList(cb.query());
        val prpp: String = keepProductId_QueryDerivedReferrer_PurchaseListParameter(vl);
        registerQueryDerivedReferrer(fn, cb.query(), "PRODUCT_ID", "PRODUCT_ID", sqpp, "purchaseList", rd, vl, prpp, op);
    }
    def keepProductId_QueryDerivedReferrer_PurchaseList(sq: PurchaseCQ): String;
    def keepProductId_QueryDerivedReferrer_PurchaseListParameter(vl: Object): String;

    /**
     * IsNull {is null}. And OnlyOnceRegistered. <br />
     * PRODUCT_ID: {PK, ID, NotNull, INTEGER(10)}
     */
    def setProductId_IsNull(): Unit = { regProductId(CK_ISN, AbstractConditionQuery.DOBJ); }

    /**
     * IsNotNull {is not null}. And OnlyOnceRegistered. <br />
     * PRODUCT_ID: {PK, ID, NotNull, INTEGER(10)}
     */
    def setProductId_IsNotNull(): Unit = { regProductId(CK_ISNN, AbstractConditionQuery.DOBJ); }

    protected def regProductId(ky: ConditionKey, vl: Any): Unit = { regQ(ky, vl, getCValueProductId(), "PRODUCT_ID"); }
    protected def getCValueProductId(): ConditionValue;

    /**
     * Equal(=). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * (商品名称)PRODUCT_NAME: {IX, NotNull, VARCHAR(50)}
     * @param productName The value of productName as equal. (NullAllowed: if null (or empty), no condition)
     */
     def setProductName_Equal(productName: String): Unit = {
        doSetProductName_Equal(fRES(productName));
    }

    protected def doSetProductName_Equal(productName: String): Unit = {
        regProductName(CK_EQ, productName);
    }

    /**
     * PrefixSearch {like 'xxx%' escape ...}. And NullOrEmptyIgnored, SeveralRegistered. <br />
     * (商品名称)PRODUCT_NAME: {IX, NotNull, VARCHAR(50)}
     * @param productName The value of productName as prefixSearch. (NullAllowed: if null (or empty), no condition)
     */
    def setProductName_PrefixSearch(productName: String): Unit = {
        setProductName_LikeSearch(productName)(_.likePrefix);
    }

    /**
     * LikeSearch with various options. (versatile) {like '%xxx%' escape ...}. And NullOrEmptyIgnored, SeveralRegistered. <br />
     * (商品名称)PRODUCT_NAME: {IX, NotNull, VARCHAR(50)} <br />
     * <pre>e.g. setProductName_LikeSearch("xxx", new <span style="color: #DD4747">LikeSearchOption</span>().likeContain());</pre>
     * @param productName The value of productName as likeSearch. (NullAllowed: if null (or empty), no condition)
     * @param optionCall The callback for option of like-search. (NotNull)
     */
    def setProductName_LikeSearch(productName: String)(optionCall: (ScrLikeSearchOption) => Unit): Unit = {
        regLSQ(CK_LS, fRES(productName), getCValueProductName(), "PRODUCT_NAME", callbackLSOP(optionCall));
    }

    /**
     * NotLikeSearch with various options. (versatile) {not like 'xxx%' escape ...} <br />
     * And NullOrEmptyIgnored, SeveralRegistered. <br />
     * (商品名称)PRODUCT_NAME: {IX, NotNull, VARCHAR(50)}
     * @param productName The value of productName as notLikeSearch. (NullAllowed: if null (or empty), no condition)
     * @param optionCall The callback for option of not-like-search. (NotNull)
     */
    def setProductName_NotLikeSearch(productName: String)(optionCall: (ScrLikeSearchOption) => Unit): Unit = {
        regLSQ(CK_NLS, fRES(productName), getCValueProductName(), "PRODUCT_NAME", callbackLSOP(optionCall));
    }

    protected def regProductName(ky: ConditionKey, vl: Any): Unit = { regQ(ky, vl, getCValueProductName(), "PRODUCT_NAME"); }
    protected def getCValueProductName(): ConditionValue;

    /**
     * Equal(=). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * (商品ハンドルコード)PRODUCT_HANDLE_CODE: {UQ, NotNull, VARCHAR(100)}
     * @param productHandleCode The value of productHandleCode as equal. (NullAllowed: if null (or empty), no condition)
     */
     def setProductHandleCode_Equal(productHandleCode: String): Unit = {
        doSetProductHandleCode_Equal(fRES(productHandleCode));
    }

    protected def doSetProductHandleCode_Equal(productHandleCode: String): Unit = {
        regProductHandleCode(CK_EQ, productHandleCode);
    }

    /**
     * NotEqual(&lt;&gt;). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * (商品ハンドルコード)PRODUCT_HANDLE_CODE: {UQ, NotNull, VARCHAR(100)}
     * @param productHandleCode The value of productHandleCode as notEqual. (NullAllowed: if null (or empty), no condition)
     */
     def setProductHandleCode_NotEqual(productHandleCode: String): Unit = {
        doSetProductHandleCode_NotEqual(fRES(productHandleCode));
    }

    protected def doSetProductHandleCode_NotEqual(productHandleCode: String): Unit = {
        regProductHandleCode(CK_NES, productHandleCode);
    }

    /**
     * InScope {in ('a', 'b')}. And NullOrEmptyIgnored, NullOrEmptyElementIgnored, SeveralRegistered. <br />
     * (商品ハンドルコード)PRODUCT_HANDLE_CODE: {UQ, NotNull, VARCHAR(100)}
     * @param productHandleCodeList The collection of productHandleCode as inScope. (NullAllowed: if null (or empty), no condition)
     */
    def setProductHandleCode_InScope(productHandleCodeList: List[String]): Unit = {
        doSetProductHandleCode_InScope(toMutableValueCollectionImplicitly(productHandleCodeList));
    }

    def doSetProductHandleCode_InScope(productHandleCodeList: Collection[String]): Unit = {
        regINS(CK_INS, cTL(productHandleCodeList), getCValueProductHandleCode(), "PRODUCT_HANDLE_CODE");
    }

    /**
     * NotInScope {not in ('a', 'b')}. And NullOrEmptyIgnored, NullOrEmptyElementIgnored, SeveralRegistered. <br />
     * (商品ハンドルコード)PRODUCT_HANDLE_CODE: {UQ, NotNull, VARCHAR(100)}
     * @param productHandleCodeList The collection of productHandleCode as notInScope. (NullAllowed: if null (or empty), no condition)
     */
    def setProductHandleCode_NotInScope(productHandleCodeList: List[String]): Unit = {
        doSetProductHandleCode_NotInScope(if (productHandleCodeList != null) { productHandleCodeList.map(_.asInstanceOf[String]).asJava } else { null });
    }

    def doSetProductHandleCode_NotInScope(productHandleCodeList: Collection[String]): Unit = {
        regINS(CK_NINS, cTL(productHandleCodeList), getCValueProductHandleCode(), "PRODUCT_HANDLE_CODE");
    }

    /**
     * PrefixSearch {like 'xxx%' escape ...}. And NullOrEmptyIgnored, SeveralRegistered. <br />
     * (商品ハンドルコード)PRODUCT_HANDLE_CODE: {UQ, NotNull, VARCHAR(100)}
     * @param productHandleCode The value of productHandleCode as prefixSearch. (NullAllowed: if null (or empty), no condition)
     */
    def setProductHandleCode_PrefixSearch(productHandleCode: String): Unit = {
        setProductHandleCode_LikeSearch(productHandleCode)(_.likePrefix);
    }

    /**
     * LikeSearch with various options. (versatile) {like '%xxx%' escape ...}. And NullOrEmptyIgnored, SeveralRegistered. <br />
     * (商品ハンドルコード)PRODUCT_HANDLE_CODE: {UQ, NotNull, VARCHAR(100)} <br />
     * <pre>e.g. setProductHandleCode_LikeSearch("xxx", new <span style="color: #DD4747">LikeSearchOption</span>().likeContain());</pre>
     * @param productHandleCode The value of productHandleCode as likeSearch. (NullAllowed: if null (or empty), no condition)
     * @param optionCall The callback for option of like-search. (NotNull)
     */
    def setProductHandleCode_LikeSearch(productHandleCode: String)(optionCall: (ScrLikeSearchOption) => Unit): Unit = {
        regLSQ(CK_LS, fRES(productHandleCode), getCValueProductHandleCode(), "PRODUCT_HANDLE_CODE", callbackLSOP(optionCall));
    }

    /**
     * NotLikeSearch with various options. (versatile) {not like 'xxx%' escape ...} <br />
     * And NullOrEmptyIgnored, SeveralRegistered. <br />
     * (商品ハンドルコード)PRODUCT_HANDLE_CODE: {UQ, NotNull, VARCHAR(100)}
     * @param productHandleCode The value of productHandleCode as notLikeSearch. (NullAllowed: if null (or empty), no condition)
     * @param optionCall The callback for option of not-like-search. (NotNull)
     */
    def setProductHandleCode_NotLikeSearch(productHandleCode: String)(optionCall: (ScrLikeSearchOption) => Unit): Unit = {
        regLSQ(CK_NLS, fRES(productHandleCode), getCValueProductHandleCode(), "PRODUCT_HANDLE_CODE", callbackLSOP(optionCall));
    }

    protected def regProductHandleCode(ky: ConditionKey, vl: Any): Unit = { regQ(ky, vl, getCValueProductHandleCode(), "PRODUCT_HANDLE_CODE"); }
    protected def getCValueProductHandleCode(): ConditionValue;

    /**
     * Equal(=). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_CATEGORY_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_CATEGORY}
     * @param productCategoryCode The value of productCategoryCode as equal. (NullAllowed: if null (or empty), no condition)
     */
     def setProductCategoryCode_Equal(productCategoryCode: String): Unit = {
        doSetProductCategoryCode_Equal(fRES(productCategoryCode));
    }

    protected def doSetProductCategoryCode_Equal(productCategoryCode: String): Unit = {
        regProductCategoryCode(CK_EQ, hSC("PRODUCT_CATEGORY_CODE", productCategoryCode, 3, "E"));
    }

    /**
     * NotEqual(&lt;&gt;). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_CATEGORY_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_CATEGORY}
     * @param productCategoryCode The value of productCategoryCode as notEqual. (NullAllowed: if null (or empty), no condition)
     */
     def setProductCategoryCode_NotEqual(productCategoryCode: String): Unit = {
        doSetProductCategoryCode_NotEqual(fRES(productCategoryCode));
    }

    protected def doSetProductCategoryCode_NotEqual(productCategoryCode: String): Unit = {
        regProductCategoryCode(CK_NES, hSC("PRODUCT_CATEGORY_CODE", productCategoryCode, 3, "E"));
    }

    /**
     * InScope {in ('a', 'b')}. And NullOrEmptyIgnored, NullOrEmptyElementIgnored, SeveralRegistered. <br />
     * PRODUCT_CATEGORY_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_CATEGORY}
     * @param productCategoryCodeList The collection of productCategoryCode as inScope. (NullAllowed: if null (or empty), no condition)
     */
    def setProductCategoryCode_InScope(productCategoryCodeList: List[String]): Unit = {
        doSetProductCategoryCode_InScope(toMutableValueCollectionImplicitly(productCategoryCodeList));
    }

    def doSetProductCategoryCode_InScope(productCategoryCodeList: Collection[String]): Unit = {
        regINS(CK_INS, cTL(productCategoryCodeList), getCValueProductCategoryCode(), "PRODUCT_CATEGORY_CODE");
    }

    /**
     * NotInScope {not in ('a', 'b')}. And NullOrEmptyIgnored, NullOrEmptyElementIgnored, SeveralRegistered. <br />
     * PRODUCT_CATEGORY_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_CATEGORY}
     * @param productCategoryCodeList The collection of productCategoryCode as notInScope. (NullAllowed: if null (or empty), no condition)
     */
    def setProductCategoryCode_NotInScope(productCategoryCodeList: List[String]): Unit = {
        doSetProductCategoryCode_NotInScope(if (productCategoryCodeList != null) { productCategoryCodeList.map(_.asInstanceOf[String]).asJava } else { null });
    }

    def doSetProductCategoryCode_NotInScope(productCategoryCodeList: Collection[String]): Unit = {
        regINS(CK_NINS, cTL(productCategoryCodeList), getCValueProductCategoryCode(), "PRODUCT_CATEGORY_CODE");
    }

    /**
     * PrefixSearch {like 'xxx%' escape ...}. And NullOrEmptyIgnored, SeveralRegistered. <br />
     * PRODUCT_CATEGORY_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_CATEGORY}
     * @param productCategoryCode The value of productCategoryCode as prefixSearch. (NullAllowed: if null (or empty), no condition)
     */
    def setProductCategoryCode_PrefixSearch(productCategoryCode: String): Unit = {
        setProductCategoryCode_LikeSearch(productCategoryCode)(_.likePrefix);
    }

    /**
     * LikeSearch with various options. (versatile) {like '%xxx%' escape ...}. And NullOrEmptyIgnored, SeveralRegistered. <br />
     * PRODUCT_CATEGORY_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_CATEGORY} <br />
     * <pre>e.g. setProductCategoryCode_LikeSearch("xxx", new <span style="color: #DD4747">LikeSearchOption</span>().likeContain());</pre>
     * @param productCategoryCode The value of productCategoryCode as likeSearch. (NullAllowed: if null (or empty), no condition)
     * @param optionCall The callback for option of like-search. (NotNull)
     */
    def setProductCategoryCode_LikeSearch(productCategoryCode: String)(optionCall: (ScrLikeSearchOption) => Unit): Unit = {
        regLSQ(CK_LS, fRES(productCategoryCode), getCValueProductCategoryCode(), "PRODUCT_CATEGORY_CODE", callbackLSOP(optionCall));
    }

    /**
     * NotLikeSearch with various options. (versatile) {not like 'xxx%' escape ...} <br />
     * And NullOrEmptyIgnored, SeveralRegistered. <br />
     * PRODUCT_CATEGORY_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_CATEGORY}
     * @param productCategoryCode The value of productCategoryCode as notLikeSearch. (NullAllowed: if null (or empty), no condition)
     * @param optionCall The callback for option of not-like-search. (NotNull)
     */
    def setProductCategoryCode_NotLikeSearch(productCategoryCode: String)(optionCall: (ScrLikeSearchOption) => Unit): Unit = {
        regLSQ(CK_NLS, fRES(productCategoryCode), getCValueProductCategoryCode(), "PRODUCT_CATEGORY_CODE", callbackLSOP(optionCall));
    }

    protected def regProductCategoryCode(ky: ConditionKey, vl: Any): Unit = { regQ(ky, vl, getCValueProductCategoryCode(), "PRODUCT_CATEGORY_CODE"); }
    protected def getCValueProductCategoryCode(): ConditionValue;

    /**
     * Equal(=). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_STATUS_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_STATUS, classification=ProductStatus}
     * @param productStatusCode The value of productStatusCode as equal. (NullAllowed: if null (or empty), no condition)
     */
    protected def setProductStatusCode_Equal(productStatusCode: String): Unit = {
        doSetProductStatusCode_Equal(fRES(productStatusCode));
    }

    /**
     * Equal(=). As ProductStatus. And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_STATUS_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_STATUS, classification=ProductStatus} <br />
     * status for product
     * @param cdef The instance of classification definition (as ENUM type). (NullAllowed: if null, no condition)
     */
    def setProductStatusCode_Equal_AsProductStatus(cdef: CDef.ProductStatus): Unit = {
        doSetProductStatusCode_Equal(if (cdef != null) { cdef.code } else { null });
    }

    /**
     * Equal(=). As ProductionSales (ONS). And OnlyOnceRegistered. <br />
     * ProductionSales
     */
    def setProductStatusCode_Equal_ProductionSales(): Unit = {
        setProductStatusCode_Equal_AsProductStatus(CDef.ProductStatus.ProductionSales);
    }

    /**
     * Equal(=). As StopProduction (PST). And OnlyOnceRegistered. <br />
     * StopProduction
     */
    def setProductStatusCode_Equal_StopProduction(): Unit = {
        setProductStatusCode_Equal_AsProductStatus(CDef.ProductStatus.StopProduction);
    }

    /**
     * Equal(=). As StopSales (SST). And OnlyOnceRegistered. <br />
     * StopSales
     */
    def setProductStatusCode_Equal_StopSales(): Unit = {
        setProductStatusCode_Equal_AsProductStatus(CDef.ProductStatus.StopSales);
    }

    protected def doSetProductStatusCode_Equal(productStatusCode: String): Unit = {
        regProductStatusCode(CK_EQ, hSC("PRODUCT_STATUS_CODE", productStatusCode, 3, "E"));
    }

    /**
     * NotEqual(&lt;&gt;). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_STATUS_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_STATUS, classification=ProductStatus}
     * @param productStatusCode The value of productStatusCode as notEqual. (NullAllowed: if null (or empty), no condition)
     */
    protected def setProductStatusCode_NotEqual(productStatusCode: String): Unit = {
        doSetProductStatusCode_NotEqual(fRES(productStatusCode));
    }

    /**
     * NotEqual(&lt;&gt;). As ProductStatus. And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * PRODUCT_STATUS_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_STATUS, classification=ProductStatus} <br />
     * status for product
     * @param cdef The instance of classification definition (as ENUM type). (NullAllowed: if null, no condition)
     */
    def setProductStatusCode_NotEqual_AsProductStatus(cdef: CDef.ProductStatus): Unit = {
        doSetProductStatusCode_NotEqual(if (cdef != null) { cdef.code } else { null });
    }

    /**
     * NotEqual(&lt;&gt;). As ProductionSales (ONS). And OnlyOnceRegistered. <br />
     * ProductionSales
     */
    def setProductStatusCode_NotEqual_ProductionSales(): Unit = {
        setProductStatusCode_NotEqual_AsProductStatus(CDef.ProductStatus.ProductionSales);
    }

    /**
     * NotEqual(&lt;&gt;). As StopProduction (PST). And OnlyOnceRegistered. <br />
     * StopProduction
     */
    def setProductStatusCode_NotEqual_StopProduction(): Unit = {
        setProductStatusCode_NotEqual_AsProductStatus(CDef.ProductStatus.StopProduction);
    }

    /**
     * NotEqual(&lt;&gt;). As StopSales (SST). And OnlyOnceRegistered. <br />
     * StopSales
     */
    def setProductStatusCode_NotEqual_StopSales(): Unit = {
        setProductStatusCode_NotEqual_AsProductStatus(CDef.ProductStatus.StopSales);
    }

    protected def doSetProductStatusCode_NotEqual(productStatusCode: String): Unit = {
        regProductStatusCode(CK_NES, hSC("PRODUCT_STATUS_CODE", productStatusCode, 3, "E"));
    }

    /**
     * InScope {in ('a', 'b')}. And NullOrEmptyIgnored, NullOrEmptyElementIgnored, SeveralRegistered. <br />
     * PRODUCT_STATUS_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_STATUS, classification=ProductStatus}
     * @param productStatusCodeList The collection of productStatusCode as inScope. (NullAllowed: if null (or empty), no condition)
     */
    def setProductStatusCode_InScope(productStatusCodeList: List[CDef.ProductStatus]): Unit = {
        doSetProductStatusCode_InScope(toMutableValueCollectionImplicitly(productStatusCodeList));
    }

    /**
     * InScope {in ('a', 'b')}. As ProductStatus. And NullOrEmptyIgnored, NullOrEmptyElementIgnored, SeveralRegistered. <br />
     * PRODUCT_STATUS_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_STATUS, classification=ProductStatus} <br />
     * status for product
     * @param cdefList The list of classification definition (as ENUM type). (NullAllowed: if null (or empty), no condition)
     */
    def setProductStatusCode_InScope_AsProductStatus(cdefList: List[CDef.ProductStatus]): Unit = {
        doSetProductStatusCode_InScope(cTStrL(cdefList.asJava));
    }

    def doSetProductStatusCode_InScope(productStatusCodeList: Collection[String]): Unit = {
        regINS(CK_INS, cTL(productStatusCodeList), getCValueProductStatusCode(), "PRODUCT_STATUS_CODE");
    }

    /**
     * NotInScope {not in ('a', 'b')}. And NullOrEmptyIgnored, NullOrEmptyElementIgnored, SeveralRegistered. <br />
     * PRODUCT_STATUS_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_STATUS, classification=ProductStatus}
     * @param productStatusCodeList The collection of productStatusCode as notInScope. (NullAllowed: if null (or empty), no condition)
     */
    def setProductStatusCode_NotInScope(productStatusCodeList: List[CDef.ProductStatus]): Unit = {
        doSetProductStatusCode_NotInScope(if (productStatusCodeList != null) { productStatusCodeList.map(_.asInstanceOf[String]).asJava } else { null });
    }

    /**
     * NotInScope {not in ('a', 'b')}. As ProductStatus. And NullOrEmptyIgnored, NullOrEmptyElementIgnored, SeveralRegistered. <br />
     * PRODUCT_STATUS_CODE: {IX, NotNull, CHAR(3), FK to PRODUCT_STATUS, classification=ProductStatus} <br />
     * status for product
     * @param cdefList The list of classification definition (as ENUM type). (NullAllowed: if null (or empty), no condition)
     */
    def setProductStatusCode_NotInScope_AsProductStatus(cdefList: List[CDef.ProductStatus]): Unit = {
        doSetProductStatusCode_NotInScope(cTStrL(cdefList.asJava));
    }

    def doSetProductStatusCode_NotInScope(productStatusCodeList: Collection[String]): Unit = {
        regINS(CK_NINS, cTL(productStatusCodeList), getCValueProductStatusCode(), "PRODUCT_STATUS_CODE");
    }

    protected def regProductStatusCode(ky: ConditionKey, vl: Any): Unit = { regQ(ky, vl, getCValueProductStatusCode(), "PRODUCT_STATUS_CODE"); }
    protected def getCValueProductStatusCode(): ConditionValue;
    
    /**
     * Equal(=). And NullIgnored, OnlyOnceRegistered. <br />
     * (定価)REGULAR_PRICE: {NotNull, INTEGER(10)}
     * @param regularPrice The value of regularPrice as equal. (NullAllowed: if null, no condition)
     */
     def setRegularPrice_Equal(regularPrice: Integer): Unit = {
        doSetRegularPrice_Equal(regularPrice);
    }

    protected def doSetRegularPrice_Equal(regularPrice: Integer): Unit = {
        regRegularPrice(CK_EQ, regularPrice);
    }

    /**
     * NotEqual(&lt;&gt;). And NullIgnored, OnlyOnceRegistered. <br />
     * (定価)REGULAR_PRICE: {NotNull, INTEGER(10)}
     * @param regularPrice The value of regularPrice as notEqual. (NullAllowed: if null, no condition)
     */
     def setRegularPrice_NotEqual(regularPrice: Integer): Unit = {
        doSetRegularPrice_NotEqual(regularPrice);
    }

    protected def doSetRegularPrice_NotEqual(regularPrice: Integer): Unit = {
        regRegularPrice(CK_NES, regularPrice);
    }

    /**
     * GreaterThan(&gt;). And NullIgnored, OnlyOnceRegistered. <br />
     * (定価)REGULAR_PRICE: {NotNull, INTEGER(10)}
     * @param regularPrice The value of regularPrice as greaterThan. (NullAllowed: if null, no condition)
     */
    def setRegularPrice_GreaterThan(regularPrice: Integer): Unit = {
        regRegularPrice(CK_GT, regularPrice);
    }

    /**
     * LessThan(&lt;). And NullIgnored, OnlyOnceRegistered. <br />
     * (定価)REGULAR_PRICE: {NotNull, INTEGER(10)}
     * @param regularPrice The value of regularPrice as lessThan. (NullAllowed: if null, no condition)
     */
    def setRegularPrice_LessThan(regularPrice: Integer): Unit = {
        regRegularPrice(CK_LT, regularPrice);
    }

    /**
     * GreaterEqual(&gt;=). And NullIgnored, OnlyOnceRegistered. <br />
     * (定価)REGULAR_PRICE: {NotNull, INTEGER(10)}
     * @param regularPrice The value of regularPrice as greaterEqual. (NullAllowed: if null, no condition)
     */
    def setRegularPrice_GreaterEqual(regularPrice: Integer): Unit = {
        regRegularPrice(CK_GE, regularPrice);
    }

    /**
     * LessEqual(&lt;=). And NullIgnored, OnlyOnceRegistered. <br />
     * (定価)REGULAR_PRICE: {NotNull, INTEGER(10)}
     * @param regularPrice The value of regularPrice as lessEqual. (NullAllowed: if null, no condition)
     */
    def setRegularPrice_LessEqual(regularPrice: Integer): Unit = {
        regRegularPrice(CK_LE, regularPrice);
    }

    /**
     * RangeOf with various options. (versatile) <br />
     * {(default) minNumber &lt;= column &lt;= maxNumber} <br />
     * And NullIgnored, OnlyOnceRegistered. <br />
     * (定価)REGULAR_PRICE: {NotNull, INTEGER(10)}
     * @param minNumber The min number of regularPrice. (NullAllowed: if null, no from-condition)
     * @param maxNumber The max number of regularPrice. (NullAllowed: if null, no to-condition)
     * @param optionCall The callback for option of range-of. (NotNull)
     */
    def setRegularPrice_RangeOf(minNumber: Integer, maxNumber: Integer)(optionCall: (RangeOfOption) => Unit): Unit = {
        regROO(minNumber, maxNumber, getCValueRegularPrice(), "REGULAR_PRICE", callbackROOP(optionCall));
    }

    /**
     * InScope {in (1, 2)}. And NullIgnored, NullElementIgnored, SeveralRegistered. <br />
     * (定価)REGULAR_PRICE: {NotNull, INTEGER(10)}
     * @param regularPriceList The collection of regularPrice as inScope. (NullAllowed: if null (or empty), no condition)
     */
    def setRegularPrice_InScope(regularPriceList: List[Int]): Unit = {
        doSetRegularPrice_InScope(toMutableValueCollectionImplicitly(regularPriceList));
    }

    protected def doSetRegularPrice_InScope(regularPriceList: Collection[Integer]): Unit = {
        regINS(CK_INS, cTL(regularPriceList), getCValueRegularPrice(), "REGULAR_PRICE");
    }

    /**
     * NotInScope {not in (1, 2)}. And NullIgnored, NullElementIgnored, SeveralRegistered. <br />
     * (定価)REGULAR_PRICE: {NotNull, INTEGER(10)}
     * @param regularPriceList The collection of regularPrice as notInScope. (NullAllowed: if null (or empty), no condition)
     */
    def setRegularPrice_NotInScope(regularPriceList: List[Integer]): Unit = {
        doSetRegularPrice_NotInScope(if (regularPriceList != null) { regularPriceList.map(_.asInstanceOf[Integer]).asJava } else { null });
    }

    protected def doSetRegularPrice_NotInScope(regularPriceList: Collection[Integer]): Unit = {
        regINS(CK_NINS, cTL(regularPriceList), getCValueRegularPrice(), "REGULAR_PRICE");
    }

    protected def regRegularPrice(ky: ConditionKey, vl: Any): Unit = { regQ(ky, vl, getCValueRegularPrice(), "REGULAR_PRICE"); }
    protected def getCValueRegularPrice(): ConditionValue;

    /**
     * Equal(=). And NullIgnored, OnlyOnceRegistered. <br />
     * REGISTER_DATETIME: {NotNull, TIMESTAMP(23, 10)}
     * @param registerDatetime The value of registerDatetime as equal. (NullAllowed: if null, no condition)
     */
    def setRegisterDatetime_Equal(registerDatetime: java.sql.Timestamp): Unit = {
        regRegisterDatetime(CK_EQ,  registerDatetime);
    }

    protected def regRegisterDatetime(ky: ConditionKey, vl: Any): Unit = { regQ(ky, vl, getCValueRegisterDatetime(), "REGISTER_DATETIME"); }
    protected def getCValueRegisterDatetime(): ConditionValue;

    /**
     * Equal(=). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * REGISTER_USER: {NotNull, VARCHAR(200)}
     * @param registerUser The value of registerUser as equal. (NullAllowed: if null (or empty), no condition)
     */
     def setRegisterUser_Equal(registerUser: String): Unit = {
        doSetRegisterUser_Equal(fRES(registerUser));
    }

    protected def doSetRegisterUser_Equal(registerUser: String): Unit = {
        regRegisterUser(CK_EQ, registerUser);
    }

    protected def regRegisterUser(ky: ConditionKey, vl: Any): Unit = { regQ(ky, vl, getCValueRegisterUser(), "REGISTER_USER"); }
    protected def getCValueRegisterUser(): ConditionValue;

    /**
     * Equal(=). And NullIgnored, OnlyOnceRegistered. <br />
     * UPDATE_DATETIME: {NotNull, TIMESTAMP(23, 10)}
     * @param updateDatetime The value of updateDatetime as equal. (NullAllowed: if null, no condition)
     */
    def setUpdateDatetime_Equal(updateDatetime: java.sql.Timestamp): Unit = {
        regUpdateDatetime(CK_EQ,  updateDatetime);
    }

    protected def regUpdateDatetime(ky: ConditionKey, vl: Any): Unit = { regQ(ky, vl, getCValueUpdateDatetime(), "UPDATE_DATETIME"); }
    protected def getCValueUpdateDatetime(): ConditionValue;

    /**
     * Equal(=). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * UPDATE_USER: {NotNull, VARCHAR(200)}
     * @param updateUser The value of updateUser as equal. (NullAllowed: if null (or empty), no condition)
     */
     def setUpdateUser_Equal(updateUser: String): Unit = {
        doSetUpdateUser_Equal(fRES(updateUser));
    }

    protected def doSetUpdateUser_Equal(updateUser: String): Unit = {
        regUpdateUser(CK_EQ, updateUser);
    }

    protected def regUpdateUser(ky: ConditionKey, vl: Any): Unit = { regQ(ky, vl, getCValueUpdateUser(), "UPDATE_USER"); }
    protected def getCValueUpdateUser(): ConditionValue;
    
    /**
     * Equal(=). And NullIgnored, OnlyOnceRegistered. <br />
     * VERSION_NO: {NotNull, BIGINT(19)}
     * @param versionNo The value of versionNo as equal. (NullAllowed: if null, no condition)
     */
     def setVersionNo_Equal(versionNo: Long): Unit = {
        doSetVersionNo_Equal(versionNo);
    }

    protected def doSetVersionNo_Equal(versionNo: Long): Unit = {
        regVersionNo(CK_EQ, versionNo);
    }

    /**
     * RangeOf with various options. (versatile) <br />
     * {(default) minNumber &lt;= column &lt;= maxNumber} <br />
     * And NullIgnored, OnlyOnceRegistered. <br />
     * VERSION_NO: {NotNull, BIGINT(19)}
     * @param minNumber The min number of versionNo. (NullAllowed: if null, no from-condition)
     * @param maxNumber The max number of versionNo. (NullAllowed: if null, no to-condition)
     * @param optionCall The callback for option of range-of. (NotNull)
     */
    def setVersionNo_RangeOf(minNumber: Long, maxNumber: Long)(optionCall: (RangeOfOption) => Unit): Unit = {
        regROO(minNumber, maxNumber, getCValueVersionNo(), "VERSION_NO", callbackROOP(optionCall));
    }

    protected def regVersionNo(ky: ConditionKey, vl: Any): Unit = { regQ(ky, vl, getCValueVersionNo(), "VERSION_NO"); }
    protected def getCValueVersionNo(): ConditionValue;

    // ===================================================================================
    //                                                                     ScalarCondition
    //                                                                     ===============
    /**
     * Prepare ScalarCondition as equal. <br />
     * {where FOO = (select max(BAR) from ...)
     * <pre>
     * cb.query().<span style="color: #DD4747">scalar_Equal()</span>.max(new SubQuery&lt;ProductCB&gt;() {
     *     public void query(ProductCB subCB) {
     *         subCB.specify().setXxx... <span style="color: #3F7E5E">// derived column for function</span>
     *         subCB.query().setYyy...
     *     }
     * });
     * </pre>
     * @return The object to set up a function. (NotNull)
     */
    def scalar_Equal(): ScrHpSSQFunction[ProductCB] = {
        return toScalaSSQFunction(xcreateSSQFunction(CK_EQ, classOf[ProductCB]));
    }

    /**
     * Prepare ScalarCondition as equal. <br />
     * {where FOO &lt;&gt; (select max(BAR) from ...)
     * <pre>
     * cb.query().<span style="color: #DD4747">scalar_NotEqual()</span>.max(new SubQuery&lt;ProductCB&gt;() {
     *     public void query(ProductCB subCB) {
     *         subCB.specify().setXxx... <span style="color: #3F7E5E">// derived column for function</span>
     *         subCB.query().setYyy...
     *     }
     * });
     * </pre>
     * @return The object to set up a function. (NotNull)
     */
    def scalar_NotEqual(): ScrHpSSQFunction[ProductCB] = {
        return toScalaSSQFunction(xcreateSSQFunction(CK_NES, classOf[ProductCB]));
    }

    /**
     * Prepare ScalarCondition as greaterThan. <br />
     * {where FOO &gt; (select max(BAR) from ...)
     * <pre>
     * cb.query().<span style="color: #DD4747">scalar_GreaterThan()</span>.max(new SubQuery&lt;ProductCB&gt;() {
     *     public void query(ProductCB subCB) {
     *         subCB.specify().setFoo... <span style="color: #3F7E5E">// derived column for function</span>
     *         subCB.query().setBar...
     *     }
     * });
     * </pre>
     * @return The object to set up a function. (NotNull)
     */
    def scalar_GreaterThan(): ScrHpSSQFunction[ProductCB] = {
        return toScalaSSQFunction(xcreateSSQFunction(CK_GT, classOf[ProductCB]));
    }

    /**
     * Prepare ScalarCondition as lessThan. <br />
     * {where FOO &lt; (select max(BAR) from ...)
     * <pre>
     * cb.query().<span style="color: #DD4747">scalar_LessThan()</span>.max(new SubQuery&lt;ProductCB&gt;() {
     *     public void query(ProductCB subCB) {
     *         subCB.specify().setFoo... <span style="color: #3F7E5E">// derived column for function</span>
     *         subCB.query().setBar...
     *     }
     * });
     * </pre>
     * @return The object to set up a function. (NotNull)
     */
    def scalar_LessThan(): ScrHpSSQFunction[ProductCB] = {
        return toScalaSSQFunction(xcreateSSQFunction(CK_LT, classOf[ProductCB]));
    }

    /**
     * Prepare ScalarCondition as greaterEqual. <br />
     * {where FOO &gt;= (select max(BAR) from ...)
     * <pre>
     * cb.query().<span style="color: #DD4747">scalar_GreaterEqual()</span>.max(new SubQuery&lt;ProductCB&gt;() {
     *     public void query(ProductCB subCB) {
     *         subCB.specify().setFoo... <span style="color: #3F7E5E">// derived column for function</span>
     *         subCB.query().setBar...
     *     }
     * });
     * </pre>
     * @return The object to set up a function. (NotNull)
     */
    def scalar_GreaterEqual(): ScrHpSSQFunction[ProductCB] = {
        return toScalaSSQFunction(xcreateSSQFunction(CK_GE, classOf[ProductCB]));
    }

    /**
     * Prepare ScalarCondition as lessEqual. <br />
     * {where FOO &lt;= (select max(BAR) from ...)
     * <pre>
     * cb.query().<span style="color: #DD4747">scalar_LessEqual()</span>.max(new SubQuery&lt;ProductCB&gt;() {
     *     public void query(ProductCB subCB) {
     *         subCB.specify().setFoo... <span style="color: #3F7E5E">// derived column for function</span>
     *         subCB.query().setBar...
     *     }
     * });
     * </pre>
     * @return The object to set up a function. (NotNull)
     */
    def scalar_LessEqual(): ScrHpSSQFunction[ProductCB] = {
        return toScalaSSQFunction(xcreateSSQFunction(CK_LE, classOf[ProductCB]));
    }

    protected def toScalaSSQFunction(function: HpSSQFunction[ProductCB]): ScrHpSSQFunction[ProductCB] =
    { new ScrHpSSQFunction(function) }

    override protected def xscalarCondition[CB <: ConditionBean](fn: String, sq: SubQuery[CB], rd: String, op: HpSSQOption[CB]): Unit = {
        assertObjectNotNull("subQuery", sq);
        val cb: ProductCB = xcreateScalarConditionCB(); sq.query(cb.asInstanceOf[CB]);
        val pp: String = keepScalarCondition(cb.query()); // for saving query-value
        op.setPartitionByCBean(xcreateScalarConditionPartitionByCB().asInstanceOf[CB]); // for using partition-by
        registerScalarCondition(fn, cb.query(), pp, rd, op);
    }
    def keepScalarCondition(sq: ProductCQ): String;

    protected def xcreateScalarConditionCB(): ProductCB = {
        val cb: ProductCB = newMyCB(); cb.xsetupForScalarCondition(this); return cb;
    }

    protected def xcreateScalarConditionPartitionByCB(): ProductCB = {
        val cb: ProductCB = newMyCB(); cb.xsetupForScalarConditionPartitionBy(this); return cb;
    }

    // ===================================================================================
    //                                                                       MyselfDerived
    //                                                                       =============
    def xsmyselfDerive(fn: String, sq: SubQuery[ProductCB], al: String, op: DerivedReferrerOption): Unit = {
        assertObjectNotNull("subQuery", sq);
        val cb: ProductCB = new ProductCB(); cb.xsetupForDerivedReferrer(this);
        try { lock(); sq.query(cb); } finally { unlock(); }
        val pp: String = keepSpecifyMyselfDerived(cb.query());
        val pk: String = "PRODUCT_ID";
        registerSpecifyMyselfDerived(fn, cb.query(), pk, pk, pp, "myselfDerived", al, op);
    }
    def keepSpecifyMyselfDerived(sq: ProductCQ): String;

    /**
     * Prepare for (Query)MyselfDerived (correlated sub-query).
     * @return The object to set up a function for myself table. (NotNull)
     */
    def myselfDerived(): ScrHpQDRFunction[ProductCB] = {
        return toScalaQDRFunction(xcreateQDRFunctionMyselfDerived(classOf[ProductCB]));
    }
    override protected def xqderiveMyselfDerived[CB <: ConditionBean](fn: String, sq: SubQuery[CB], rd: String, vl: Object, op: DerivedReferrerOption): Unit = {
        assertObjectNotNull("subQuery", sq);
        val cb: ProductCB = new ProductCB(); cb.xsetupForDerivedReferrer(this); sq.query(cb.asInstanceOf[CB]);
        val pk: String = "PRODUCT_ID";
        val sqpp: String = keepQueryMyselfDerived(cb.query()); // for saving query-value.
        val prpp: String = keepQueryMyselfDerivedParameter(vl);
        registerQueryMyselfDerived(fn, cb.query(), pk, pk, sqpp, "myselfDerived", rd, vl, prpp, op);
    }
    def keepQueryMyselfDerived(sq: ProductCQ): String;
    def keepQueryMyselfDerivedParameter(vl: Object): String;

    // ===================================================================================
    //                                                                        MyselfExists
    //                                                                        ============
    /**
     * Prepare for MyselfExists (correlated sub-query).
     * @param subQuery The implementation of sub-query. (NotNull)
     */
    def myselfExists(subQuery: (ProductCB) => Unit): Unit = {
        assertObjectNotNull("subQuery", subQuery);
        val cb: ProductCB = new ProductCB(); cb.xsetupForMyselfExists(this);
        try { lock(); subQuery(cb); } finally { unlock(); }
        val pp: String = keepMyselfExists(cb.query());
        registerMyselfExists(cb.query(), pp);
    }
    def keepMyselfExists(sq: ProductCQ): String;

    // ===================================================================================
    //                                                                       MyselfInScope
    //                                                                       =============
    /**
     * Prepare for MyselfInScope (sub-query).
     * @param subQuery The implementation of sub-query. (NotNull)
     */
    def myselfInScope(subQuery: (ProductCB) => Unit): Unit = {
        assertObjectNotNull("subQuery", subQuery);
        val cb: ProductCB = new ProductCB(); cb.xsetupForMyselfInScope(this);
        try { lock(); subQuery(cb); } finally { unlock(); }
        val pp: String = keepMyselfInScope(cb.query());
        registerMyselfInScope(cb.query(), pp);
    }
    def keepMyselfInScope(sq: ProductCQ): String;

    // ===================================================================================
    //                                                                            Order By
    //                                                                            ========
    /**
     * Order along manual ordering information.
     * <pre>
     * MemberCB cb = new MemberCB();
     * ManualOrderBean mob = new ManualOrderBean();
     * mob.<span style="color: #DD4747">when_GreaterEqual</span>(priorityDate); <span style="color: #3F7E5E">// e.g. 2000/01/01</span>
     * cb.query().addOrderBy_Birthdate_Asc().<span style="color: #DD4747">withManualOrder(mob)</span>;
     * <span style="color: #3F7E5E">// order by </span>
     * <span style="color: #3F7E5E">//   case</span>
     * <span style="color: #3F7E5E">//     when BIRTHDATE &gt;= '2000/01/01' then 0</span>
     * <span style="color: #3F7E5E">//     else 1</span>
     * <span style="color: #3F7E5E">//   end asc, ...</span>
     *
     * MemberCB cb = new MemberCB();
     * ManualOrderBean mob = new ManualOrderBean();
     * mob.<span style="color: #DD4747">when_Equal</span>(CDef.MemberStatus.Withdrawal);
     * mob.<span style="color: #DD4747">when_Equal</span>(CDef.MemberStatus.Formalized);
     * mob.<span style="color: #DD4747">when_Equal</span>(CDef.MemberStatus.Provisional);
     * cb.query().addOrderBy_MemberStatusCode_Asc().<span style="color: #DD4747">withManualOrder(mob)</span>;
     * <span style="color: #3F7E5E">// order by </span>
     * <span style="color: #3F7E5E">//   case</span>
     * <span style="color: #3F7E5E">//     when MEMBER_STATUS_CODE = 'WDL' then 0</span>
     * <span style="color: #3F7E5E">//     when MEMBER_STATUS_CODE = 'FML' then 1</span>
     * <span style="color: #3F7E5E">//     when MEMBER_STATUS_CODE = 'PRV' then 2</span>
     * <span style="color: #3F7E5E">//     else 3</span>
     * <span style="color: #3F7E5E">//   end asc, ...</span>
     * </pre>
     * <p>This function with Union is unsupported!</p>
     * <p>The order values are bound (treated as bind parameter).</p>
     * @param mob The bean of manual order containing order values. (NotNull)
     */
    def withManualOrder(mobCall: (ScrManualOrderBean) => Unit): Unit = { // is user public!
        assertObjectNotNull("withManualOrder(mobCall)", mobCall);
        xdoWithManualOrder(callbackMOB(mobCall));
    }

    // ===================================================================================
    //                                                                       Create Option
    //                                                                       =============
    protected def callbackLSOP(optionCall: (ScrLikeSearchOption) => Unit): LikeSearchOption =
    { val op = createLikeSearchOption(); optionCall(op); return op; }
    protected def createLikeSearchOption(): ScrLikeSearchOption = { new ScrLikeSearchOption() }

    protected def callbackFTOP(optionCall: (ScrFromToOption) => Unit): FromToOption =
    { val op = createFromToOption(); optionCall(op); return op; }
    protected def createFromToOption(): ScrFromToOption = { new ScrFromToOption() }

    protected def callbackROOP(optionCall: (ScrRangeOfOption) => Unit): RangeOfOption =
    { val op = createRangeOfOption(); optionCall(op); return op; }
    protected def createRangeOfOption(): ScrRangeOfOption = { new ScrRangeOfOption() }

    protected def callbackMOB(mobCall: (ScrManualOrderBean) => Unit): ManualOrderBean =
    { val mob = createManualOrderBean(); mobCall(mob); return mob; }
    protected def createManualOrderBean(): ScrManualOrderBean = { new ScrManualOrderBean() }

    // ===================================================================================
    //                                                                        Invoke Query
    //                                                                        ============
    override protected def xfilterInvokeQueryParameterType(colName: String, ckey: String, parameterType: Class[_]): Class[_] =
    { if (classOf[Collection[_]].isAssignableFrom(parameterType)) { classOf[List[_]] } else { parameterType } }

    override protected def xfilterInvokeQueryParameterValue(colName: String, ckey: String, parameterValue: Object): Object =
    { if (parameterValue.isInstanceOf[Collection[_]]) { toScalaList(parameterValue.asInstanceOf[Collection[_]]) } else { parameterValue } }

    // ===================================================================================
    //                                                                        Scala Helper
    //                                                                        ============
    protected def toScalaList[ENTITY](javaList: Collection[ENTITY]): scala.collection.immutable.List[ENTITY] = {
        if (javaList == null) { scala.collection.immutable.List() }
        return scala.collection.immutable.List.fromArray(javaList.toArray()).asInstanceOf[scala.collection.immutable.List[ENTITY]];
    }

    protected def toMutableValueCollectionImplicitly[SCALA, JAVA](ls: List[SCALA]): Collection[JAVA] =
    { if (ls != null) { ls.map(_.asInstanceOf[JAVA]).asJava } else { null } }

    protected def toScalaQDRFunction[CB <: ConditionBean](function: HpQDRFunction[CB]): ScrHpQDRFunction[CB] =
    { new ScrHpQDRFunction[CB](function) }

    // ===================================================================================
    //                                                                       Very Internal
    //                                                                       =============
    protected def newMyCB(): ProductCB = {
        return new ProductCB();
    }
    // very internal (for suppressing warn about 'Not Use Import')
    protected def xabCQ(): String = { return classOf[ProductCQ].getName(); }
    protected def xabLSO(): String = { return classOf[LikeSearchOption].getName(); }
    protected def xabSSQS(): String = { return classOf[HpSSQSetupper[_]].getName(); }
}
