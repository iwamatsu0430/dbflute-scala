package com.example.dbflute.scala.dbflute.bsbhv;

import scala.collection.immutable;
import scala.collection.JavaConverters._;

import java.util.Collection;
import java.util.List;
import java.util.ArrayList;

import org.seasar.dbflute._;
import org.seasar.dbflute.bhv._;
import org.seasar.dbflute.bhv.AbstractBehaviorReadable._;
import org.seasar.dbflute.bhv.AbstractBehaviorWritable._;
import org.seasar.dbflute.cbean._;
import org.seasar.dbflute.cbean.chelper._;
import org.seasar.dbflute.dbmeta.DBMeta;
import org.seasar.dbflute.exception._;
import org.seasar.dbflute.util._;
import org.seasar.dbflute.outsidesql.executor._;
import com.example.dbflute.scala.dbflute.allcommon._;
import com.example.dbflute.scala.dbflute.exbhv._;
import com.example.dbflute.scala.dbflute.bsbhv.loader._;
import com.example.dbflute.scala.dbflute.exentity._;
import com.example.dbflute.scala.dbflute.bsentity.dbmeta._;
import com.example.dbflute.scala.dbflute.cbean._;

/**
 * The behavior of (商品ステータス)PRODUCT_STATUS as TABLE. <br />
 * <pre>
 * [primary key]
 *     PRODUCT_STATUS_CODE
 *
 * [column]
 *     PRODUCT_STATUS_CODE, PRODUCT_STATUS_NAME, DISPLAY_ORDER
 *
 * [sequence]
 *     
 *
 * [identity]
 *     
 *
 * [version-no]
 *     
 *
 * [foreign table]
 *     
 *
 * [referrer table]
 *     PRODUCT
 *
 * [foreign property]
 *     
 *
 * [referrer property]
 *     productList
 * </pre>
 * @author DBFlute(AutoGenerator)
 */
abstract class BsProductStatusBhv extends AbstractBehaviorWritable {

    // ===================================================================================
    //                                                                          Definition
    //                                                                          ==========
    /*df:beginQueryPath*/
    /*df:endQueryPath*/

    // ===================================================================================
    //                                                                          Table name
    //                                                                          ==========
    /** @return The name on database of table. (NotNull) */
    def getTableDbName(): String = { return "PRODUCT_STATUS"; }

    // ===================================================================================
    //                                                                              DBMeta
    //                                                                              ======
    /** {@inheritDoc} */
    def getDBMeta(): DBMeta = { return ProductStatusDbm; }

    // ===================================================================================
    //                                                                        New Instance
    //                                                                        ============
    /** {@inheritDoc} */
    def newEntity(): DbleProductStatus = { return new DbleProductStatus(); }

    /** {@inheritDoc} */
    def newConditionBean(): ProductStatusCB = { return new ProductStatusCB(); }

    // ===================================================================================
    //                                                                        Count Select
    //                                                                        ============
    /**
     * Select the count of uniquely-selected records by the condition-bean. {IgnorePagingCondition, IgnoreSpecifyColumn}<br />
     * SpecifyColumn is ignored but you can use it only to remove text type column for union's distinct.
     * <pre>
     * ProductStatusCB cb = new ProductStatusCB();
     * cb.query().setFoo...(value);
     * int count = productStatusBhv.<span style="color: #DD4747">selectCount</span>(cb);
     * </pre>
     * @param cb The condition-bean of DbleProductStatus. (NotNull)
     * @return The count for the condition. (NotMinus)
     */
    def selectCount(cbCall: (ProductStatusCB) => Unit): Int = {
        return facadeSelectCount(callbackCB(cbCall));
    }

    protected def facadeSelectCount(cb: ProductStatusCB): Int = {
        return doSelectCountUniquely(cb);
    }

    protected def doSelectCountUniquely(cb: ProductStatusCB): Int = { // called by selectCount(cb)
        assertCBStateValid(cb);
        return delegateSelectCountUniquely(cb);
    }

    protected def doSelectCountPlainly(cb: ProductStatusCB): Int = { // called by selectPage(cb)
        assertCBStateValid(cb);
        return delegateSelectCountPlainly(cb);
    }

    override protected def doReadCount(cb: ConditionBean): Int = {
        return facadeSelectCount(downcast(cb));
    }

    // ===================================================================================
    //                                                                       Entity Select
    //                                                                       =============
    /**
     * Select the entity by the condition-bean. <br />
     * It returns not-null optional entity, so you should ... <br />
     * <span style="color: #AD4747; font-size: 120%">If the data always exists as your business rule, get() without check.</span> <br />
     * <span style="color: #AD4747; font-size: 120%">If it might be no data, get() after check by isPresent() or orElse(), ...</span>
     * <pre>
     * ProductStatusCB cb = new ProductStatusCB();
     * cb.query().setFoo...(value);
     * OptionalEntity&lt;DbleProductStatus&gt; entity = productStatusBhv.<span style="color: #DD4747">selectEntity</span>(cb);
     *
     * <span style="color: #3F7E5E">// if the data always exists as your business rule</span>
     * entity.<span style="color: #DD4747">required</span>(productStatus -&gt; {
     *     ...
     * });
     * DbleProductStatus productStatus = entity.entity.<span style="color: #DD4747">get()</span>;
     *
     * <span style="color: #3F7E5E">// if it might be no data, ifPresent(), isPresent(), ...</span>
     * entity.<span style="color: #DD4747">ifPresent</span>(productStatus -&gt; {
     *     ...
     * });
     * if (entity.entity.<span style="color: #DD4747">isPresent()</span>) {
     *     DbleProductStatus productStatus = entity.entity.<span style="color: #DD4747">get()</span>;
     * } else {
     *     ...
     * }
     * </pre>
     * @param cbCall The callback for condition-bean of ProductStatus. (NotNull)
     * @param loaderCall The callback for referrer loader of ProductStatus. (NoArgAllowed: then no loading)
     * @return The optional entity selected by the condition. (NotNull: if no data, empty entity)
     * @exception EntityAlreadyDeletedException When get() of return value is called and the value is null, which means entity has already been deleted (point is not found).
     * @exception EntityDuplicatedException When the entity has been duplicated.
     * @exception SelectEntityConditionNotFoundException When the condition for selecting an entity is not found.
     */
    def selectEntity(cbCall: (ProductStatusCB) => Unit)(implicit loaderCall: (LoaderOfProductStatus) => Unit = null): Option[ProductStatus] = {
        return facadeSelectEntity(callbackCB(cbCall))(loaderCall).map(_.toImmutable);
    }

    protected def facadeSelectEntity(cb: ProductStatusCB)(loaderCall: (LoaderOfProductStatus) => Unit = null): Option[DbleProductStatus] = {
        return doSelectOptionalEntity(cb, typeOfSelectedEntity())(loaderCall);
    }

    protected def doSelectEntity[ENTITY <: DbleProductStatus](cb: ProductStatusCB, tp: Class[ENTITY])(loaderCall: (LoaderOfProductStatus) => Unit = null): ENTITY = {
        assertCBStateValid(cb); assertObjectNotNull("entityType", tp);
        val dble = helpSelectEntityInternally(cb, tp);
        if (dble != null) {
            callbackLoader(DfCollectionUtil.newArrayList(dble.asInstanceOf[DbleProductStatus]), loaderCall);
        }
        return dble;
    }

    protected def doSelectOptionalEntity[ENTITY <: DbleProductStatus](cb: ProductStatusCB, tp: Class[ENTITY])(loaderCall: (LoaderOfProductStatus) => Unit = null): Option[ENTITY] = {
        return Option.apply(doSelectEntity(cb, tp)(loaderCall));
    }

    protected def doReadEntity(cb: ConditionBean): Entity = { facadeSelectEntity(downcast(cb))().orNull }

    /**
     * Select the entity by the condition-bean with deleted check. <br />
     * <span style="color: #AD4747; font-size: 120%">If the data always exists as your business rule, this method is good.</span>
     * <pre>
     * ProductStatusCB cb = new ProductStatusCB();
     * cb.query().setFoo...(value);
     * DbleProductStatus productStatus = productStatusBhv.<span style="color: #DD4747">selectEntityWithDeletedCheck</span>(cb);
     * ... = productStatus.get...(); <span style="color: #3F7E5E">// the entity always be not null</span>
     * </pre>
     * @param cbCall The callback for condition-bean of ProductStatus. (NotNull)
     * @param loaderCall The callback for referrer loader of ProductStatus. (NoArgAllowed: then no loading)
     * @return The entity selected by the condition. (NotNull: if no data, throws exception)
     * @exception EntityAlreadyDeletedException When the entity has already been deleted. (point is not found)
     * @exception EntityDuplicatedException When the entity has been duplicated.
     * @exception SelectEntityConditionNotFoundException When the condition for selecting an entity is not found.
     */
    def selectEntityWithDeletedCheck(cbCall: (ProductStatusCB) => Unit)(implicit loaderCall: (LoaderOfProductStatus) => Unit = null): ProductStatus = {
        return facadeSelectEntityWithDeletedCheck(callbackCB(cbCall))(loaderCall).toImmutable;
    }

    protected def facadeSelectEntityWithDeletedCheck(cb: ProductStatusCB)(loaderCall: (LoaderOfProductStatus) => Unit = null): DbleProductStatus = {
        return doSelectEntityWithDeletedCheck(cb, typeOfSelectedEntity())(loaderCall);
    }

    protected def doSelectEntityWithDeletedCheck[ENTITY <: DbleProductStatus](cb: ProductStatusCB, tp: Class[ENTITY])(loaderCall: (LoaderOfProductStatus) => Unit = null): ENTITY = {
        assertCBStateValid(cb); assertObjectNotNull("entityType", tp);
        val dble = helpSelectEntityWithDeletedCheckInternally(cb, tp);
        callbackLoader(DfCollectionUtil.newArrayList(dble.asInstanceOf[DbleProductStatus]), loaderCall);
        return dble;
    }

    protected def doReadEntityWithDeletedCheck(cb: ConditionBean): Entity = { facadeSelectEntityWithDeletedCheck(downcast(cb))() }

    /**
     * Select the entity by the primary-key value.
     * @param productStatusCode (商品ステータスコード): PK, NotNull, CHAR(3), classification=ProductStatus. (NotNull)
     * @return The optional entity selected by the PK. (NotNull: if no data, empty entity)
     * @exception EntityAlreadyDeletedException When get(), required() of return value is called and the value is null, which means entity has already been deleted (not found).
     * @exception EntityDuplicatedException When the entity has been duplicated.
     * @exception SelectEntityConditionNotFoundException When the condition for selecting an entity is not found.
     */
    def selectByPK(productStatusCode: CDef.ProductStatus): Option[ProductStatus] = {
        return facadeSelectByPK(productStatusCode).map(_.toImmutable);
    }

    protected def facadeSelectByPK(productStatusCode: CDef.ProductStatus): Option[DbleProductStatus] = {
        return doSelectByPK(productStatusCode, typeOfSelectedEntity());
    }

    protected def doSelectByPK[ENTITY <: DbleProductStatus](productStatusCode: CDef.ProductStatus, tp: Class[ENTITY]): Option[ENTITY] = {
        return Option.apply(doSelectEntity(xprepareCBAsPK(productStatusCode), tp)());
    }

    protected def xprepareCBAsPK(productStatusCode: CDef.ProductStatus): ProductStatusCB = {
        assertObjectNotNull("productStatusCode", productStatusCode);
        return newConditionBean().acceptPK(productStatusCode);
    }

    /**
     * Select the entity by the unique-key value.
     * @param displayOrder (表示順): UQ, NotNull, INTEGER(10). (NotNull)
     * @return The optional entity selected by the unique key. (NotNull: if no data, empty entity)
     * @exception EntityAlreadyDeletedException When get(), required() of return value is called and the value is null, which means entity has already been deleted (not found).
     * @exception EntityDuplicatedException When the entity has been duplicated.
     * @exception SelectEntityConditionNotFoundException When the condition for selecting an entity is not found.
     */
    def selectByUniqueOf(displayOrder: Integer): Option[ProductStatus] = {
        return facadeSelectByUniqueOf(displayOrder).map(_.toImmutable);
    }

    protected def facadeSelectByUniqueOf(displayOrder: Integer): Option[DbleProductStatus] = {
        return doSelectByUniqueOf(displayOrder, typeOfSelectedEntity());
    }

    protected def doSelectByUniqueOf[ENTITY <: DbleProductStatus](displayOrder: Integer, tp: Class[ENTITY]): Option[ENTITY] = {
        return Option.apply(doSelectEntity(xprepareCBAsUniqueOf(displayOrder), tp)());
    }

    protected def xprepareCBAsUniqueOf(displayOrder: Integer): ProductStatusCB = {
        assertObjectNotNull("displayOrder", displayOrder);
        return newConditionBean().acceptUniqueOf(displayOrder);
    }

    // ===================================================================================
    //                                                                         List Select
    //                                                                         ===========
    /**
     * Select the list as result bean.
     * <pre>
     * ProductStatusCB cb = new ProductStatusCB();
     * cb.query().setFoo...(value);
     * cb.query().addOrderBy_Bar...();
     * List&lt;${ProductStatus}&gt; productStatusList = productStatusBhv.<span style="color: #DD4747">selectList</span>(cb);
     * productStatusList.foreach(productStatus =>
     *     ... = productStatus...;
     * }
     * </pre>
     * @param cbCall The callback for condition-bean of ProductStatus. (NotNull)
     * @param loaderCall The callback for referrer loader of ProductStatus. (NoArgAllowed: then no loading)
     * @return The result bean of selected list. (NotNull: if no data, returns empty list)
     * @exception DangerousResultSizeException When the result size is over the specified safety size.
     */
    def selectList(cbCall: (ProductStatusCB) => Unit)(implicit loaderCall: (LoaderOfProductStatus) => Unit = null): immutable.List[ProductStatus] = {
        return toImmutableEntityList(facadeSelectList(callbackCB(cbCall))(loaderCall));
    }

    protected def facadeSelectList(cb: ProductStatusCB)(loaderCall: (LoaderOfProductStatus) => Unit = null): ListResultBean[DbleProductStatus] = {
        return doSelectList(cb, typeOfSelectedEntity())(loaderCall);
    }

    protected def doSelectList[ENTITY <: DbleProductStatus](cb: ProductStatusCB, tp: Class[ENTITY])(loaderCall: (LoaderOfProductStatus) => Unit = null): ListResultBean[ENTITY] = {
        val dbleList = helpSelectListInternally(cb, tp);
        callbackLoader(dbleList.asInstanceOf[List[DbleProductStatus]], loaderCall);
        return dbleList;
    }

    protected def doReadList(cb: ConditionBean): ListResultBean[_ <: Entity] = { facadeSelectList(downcast(cb))() }

    override protected def isSuppressSpecifyDerivedReferrerEntityPropertyCheck(): Boolean = { true }

    // ===================================================================================
    //                                                                         Page Select
    //                                                                         ===========
    /**
     * Select the page as result bean. <br />
     * (both count-select and paging-select are executed)
     * <pre>
     * ProductStatusCB cb = new ProductStatusCB();
     * cb.query().setFoo...(value);
     * cb.query().addOrderBy_Bar...();
     * cb.<span style="color: #DD4747">paging</span>(20, 3); <span style="color: #3F7E5E">// 20 records per a page and current page number is 3</span>
     * PagingResultBean&lt;DbleProductStatus&gt; page = productStatusBhv.<span style="color: #DD4747">selectPage</span>(cb);
     * int allRecordCount = page.getAllRecordCount();
     * int allPageCount = page.getAllPageCount();
     * boolean isExistPrePage = page.isExistPrePage();
     * boolean isExistNextPage = page.isExistNextPage();
     * ...
     * for (DbleProductStatus productStatus : page) {
     *     ... = productStatus.get...();
     * }
     * </pre>
     * @param cbCall The callback for condition-bean of ProductStatus. (NotNull)
     * @param loaderCall The callback for referrer loader of ProductStatus. (NoArgAllowed: then no loading)
     * @return The result bean of selected page. (NotNull: if no data, returns bean as empty list)
     * @exception DangerousResultSizeException When the result size is over the specified safety size.
     */
    def selectPage(cbCall: (ProductStatusCB) => Unit)(implicit loaderCall: (LoaderOfProductStatus) => Unit = null): ScrPagingView[ProductStatus] = {
        return newPagingView(facadeSelectPage(callbackCB(cbCall))(loaderCall));
    }

    protected def facadeSelectPage(cb: ProductStatusCB)(loaderCall: (LoaderOfProductStatus) => Unit = null): PagingResultBean[DbleProductStatus] = {
        return doSelectPage(cb, typeOfSelectedEntity())(loaderCall);
    }

    protected def doSelectPage[ENTITY <: DbleProductStatus](cb: ProductStatusCB, tp: Class[ENTITY])(loaderCall: (LoaderOfProductStatus) => Unit = null): PagingResultBean[ENTITY] = {
        return helpSelectPageInternally(cb, tp);
    }

    protected def doReadPage(cb: ConditionBean): PagingResultBean[_ <: Entity] = { facadeSelectPage(downcast(cb))() }

    // ===================================================================================
    //                                                                       Cursor Select
    //                                                                       =============
    /**
     * Select the cursor by the condition-bean.
     * <pre>
     * ProductStatusCB cb = new ProductStatusCB();
     * cb.query().setFoo...(value);
     * productStatusBhv.<span style="color: #DD4747">selectCursor</span>(cb, new EntityRowHandler&lt;DbleProductStatus&gt;() {
     *     public void handle(DbleProductStatus entity) {
     *         ... = entity.getFoo...();
     *     }
     * });
     * </pre>
     * @param cb The condition-bean of DbleProductStatus. (NotNull)
     * @param entityRowHandler The handler of entity row of DbleProductStatus. (NotNull)
     */
    def selectCursor(cbCall: (ProductStatusCB) => Unit)(rowCall: (ProductStatus) => Unit): Unit = {
        facadeSelectCursor(callbackCB(cbCall), new EntityRowHandler[DbleProductStatus]() {
            def handle(entity: DbleProductStatus): Unit = { rowCall(entity.toImmutable) }
        });
    }

    protected def facadeSelectCursor(cb: ProductStatusCB, handler: EntityRowHandler[DbleProductStatus]): Unit = {
        doSelectCursor(cb, handler, typeOfSelectedEntity());
    }

    protected def doSelectCursor[ENTITY <: DbleProductStatus](cb: ProductStatusCB, handler: EntityRowHandler[ENTITY], tp: Class[ENTITY]): Unit = {
        helpSelectCursorInternally(cb, handler, tp);
    }

    // ===================================================================================
    //                                                                       Scalar Select
    //                                                                       =============
    /**
     * Select the scalar value derived by a function from uniquely-selected records. <br />
     * You should call a function method after this method called like as follows:
     * <pre>
     * productStatusBhv.<span style="color: #DD4747">scalarSelect</span>(Date.class).max(new ScalarQuery() {
     *     public void query(ProductStatusCB cb) {
     *         cb.specify().<span style="color: #DD4747">columnFooDatetime()</span>; <span style="color: #3F7E5E">// required for a function</span>
     *         cb.query().setBarName_PrefixSearch("S");
     *     }
     * });
     * </pre>
     * @param <RESULT> The type of result.
     * @param resultType The type of result. (NotNull)
     * @return The scalar function object to specify function for scalar value. (NotNull)
     */
    def scalarSelect[RESULT](resultType: Class[RESULT]): ScrHpSLSFunction[ProductStatusCB, RESULT] = {
        return toScalaSLSFunction[RESULT](facadeScalarSelect(resultType));
    }

    protected def toScalaSLSFunction[RESULT](function: HpSLSFunction[ProductStatusCB, RESULT]): ScrHpSLSFunction[ProductStatusCB, RESULT] =
    { new ScrHpSLSFunction[ProductStatusCB, RESULT](function) }

    protected def facadeScalarSelect[RESULT](resultType: Class[RESULT]): HpSLSFunction[ProductStatusCB, RESULT] = {
        return doScalarSelect(resultType, newConditionBean());
    }

    protected def doScalarSelect[RESULT, CB <: ProductStatusCB](tp: Class[RESULT], cb: CB): HpSLSFunction[CB, RESULT] = {
        assertObjectNotNull("resultType", tp); assertCBStateValid(cb);
        cb.xsetupForScalarSelect(); cb.getSqlClause().disableSelectIndex(); // for when you use union
        return createSLSFunction[CB, RESULT](cb, tp, createHpSLSExecutor());
    }

    protected def doReadScalar[RESULT](tp: Class[RESULT]): HpSLSFunction[_ <: ConditionBean, RESULT] = { facadeScalarSelect(tp) }

    // ===================================================================================
    //                                                                            Sequence
    //                                                                            ========
    @Override
    protected def doReadNextVal(): Number = {
        val msg: String = "This table is NOT related to sequence: " + getTableDbName();
        throw new UnsupportedOperationException(msg);
    }

    // ===================================================================================
    //                                                                       Load Referrer
    //                                                                       =============
    /**
     * Load referrer of productList by the set-upper of referrer. <br />
     * (商品)PRODUCT by PRODUCT_STATUS_CODE, named 'productList'.
     * <pre>
     * productStatusBhv.<span style="color: #DD4747">loadProductList</span>(productStatusList, new ReferrerConditionSetupper&lt;ProductCB&gt;() {
     *     public void setup(ProductCB cb) {
     *         cb.setupSelect...();
     *         cb.query().setFoo...(value);
     *         cb.query().addOrderBy_Bar...();
     *     }
     * }); <span style="color: #3F7E5E">// you can load nested referrer from here</span>
     * <span style="color: #3F7E5E">//}).withNestedList(referrerList -&gt {</span>
     * <span style="color: #3F7E5E">//    ...</span>
     * <span style="color: #3F7E5E">//});</span>
     * for (DbleProductStatus productStatus : productStatusList) {
     *     ... = productStatus.<span style="color: #DD4747">getProductList()</span>;
     * }
     * </pre>
     * About internal policy, the value of primary key (and others too) is treated as case-insensitive. <br />
     * The condition-bean, which the set-upper provides, has settings before callback as follows:
     * <pre>
     * cb.query().setProductStatusCode_InScope(pkList);
     * cb.query().addOrderBy_ProductStatusCode_Asc();
     * </pre>
     * @param productStatusList The entity list of productStatus. (NotNull)
     * @param setupper The callback to set up referrer condition-bean for loading referrer. (NotNull)
     * @return The callback interface which you can load nested referrer by calling withNestedReferrer(). (NotNull)
     */
    def loadProductList(productStatusList: List[DbleProductStatus], setupCall: (ProductCB) => Unit): NestedReferrerListGateway[DbleProduct] = {
        assertObjectNotNull("productStatusList", productStatusList); assertObjectNotNull("setupCall", setupCall);
        val setupper = new ReferrerConditionSetupper[ProductCB]() { def setup(referrerCB: ProductCB): Unit = { setupCall(referrerCB); } }
        return doLoadProductList(productStatusList, new LoadReferrerOption[ProductCB, DbleProduct]().xinit(setupper));
    }

    /**
     * Load referrer of productList by the set-upper of referrer. <br />
     * (商品)PRODUCT by PRODUCT_STATUS_CODE, named 'productList'.
     * <pre>
     * productStatusBhv.<span style="color: #DD4747">loadProductList</span>(productStatusList, new ReferrerConditionSetupper&lt;ProductCB&gt;() {
     *     public void setup(ProductCB cb) {
     *         cb.setupSelect...();
     *         cb.query().setFoo...(value);
     *         cb.query().addOrderBy_Bar...();
     *     }
     * }); <span style="color: #3F7E5E">// you can load nested referrer from here</span>
     * <span style="color: #3F7E5E">//}).withNestedList(referrerList -&gt {</span>
     * <span style="color: #3F7E5E">//    ...</span>
     * <span style="color: #3F7E5E">//});</span>
     * ... = productStatus.<span style="color: #DD4747">getProductList()</span>;
     * </pre>
     * About internal policy, the value of primary key (and others too) is treated as case-insensitive. <br />
     * The condition-bean, which the set-upper provides, has settings before callback as follows:
     * <pre>
     * cb.query().setProductStatusCode_InScope(pkList);
     * cb.query().addOrderBy_ProductStatusCode_Asc();
     * </pre>
     * @param productStatus The entity of productStatus. (NotNull)
     * @param setupper The callback to set up referrer condition-bean for loading referrer. (NotNull)
     * @return The callback interface which you can load nested referrer by calling withNestedReferrer(). (NotNull)
     */
    def loadProductList(productStatus: DbleProductStatus, setupCall: (ProductCB) => Unit): NestedReferrerListGateway[DbleProduct] = {
        assertObjectNotNull("productStatus", productStatus); assertObjectNotNull("setupCall", setupCall);
        val setupper = new ReferrerConditionSetupper[ProductCB]() { def setup(referrerCB: ProductCB): Unit = { setupCall(referrerCB); } }
        return doLoadProductList(xnewLRLs(productStatus), new LoadReferrerOption[ProductCB, DbleProduct]().xinit(setupper));
    }

    protected def doLoadProductList(productStatusList: List[DbleProductStatus], option: LoadReferrerOption[ProductCB, DbleProduct]): NestedReferrerListGateway[DbleProduct] = {
        return helpLoadReferrerInternally(productStatusList, option, "productList");
    }

    // ===================================================================================
    //                                                                   Pull out Relation
    //                                                                   =================
    // ===================================================================================
    //                                                                      Extract Column
    //                                                                      ==============
    /**
     * Extract the value list of (single) primary key productStatusCode.
     * @param productStatusList The list of productStatus. (NotNull, EmptyAllowed)
     * @return The list of the column value. (NotNull, EmptyAllowed, NotNullElement)
     */
    def extractProductStatusCodeList(productStatusList: immutable.List[ProductStatus]): immutable.List[CDef.ProductStatus] = {
        val plainList = helpExtractListInternally(toDBableEntityList(productStatusList), "productStatusCode");
        return toScalaList(plainList).map(_.asInstanceOf[CDef.ProductStatus]);
    }

    /**
     * Extract the value list of (single) unique key displayOrder.
     * @param productStatusList The list of productStatus. (NotNull, EmptyAllowed)
     * @return The list of the column value. (NotNull, EmptyAllowed, NotNullElement)
     */
    def extractDisplayOrderList(productStatusList: immutable.List[ProductStatus]): immutable.List[Int] = {
        val plainList = helpExtractListInternally(toDBableEntityList(productStatusList), "displayOrder");
        return toScalaList(plainList).map(_.asInstanceOf[Int]);
    }

    // ===================================================================================
    //                                                                       Entity Update
    //                                                                       =============
    /**
     * Insert the entity modified-only. (DefaultConstraintsEnabled)
     * <pre>
     * DbleProductStatus productStatus = new DbleProductStatus();
     * <span style="color: #3F7E5E">// if auto-increment, you don't need to set the PK value</span>
     * productStatus.setFoo...(value);
     * productStatus.setBar...(value);
     * <span style="color: #3F7E5E">// you don't need to set values of common columns</span>
     * <span style="color: #3F7E5E">//productStatus.setRegisterUser(value);</span>
     * <span style="color: #3F7E5E">//productStatus.set...;</span>
     * productStatusBhv.<span style="color: #DD4747">insert</span>(productStatus);
     * ... = productStatus.getPK...(); <span style="color: #3F7E5E">// if auto-increment, you can get the value after</span>
     * </pre>
     * <p>While, when the entity is created by select, all columns are registered.</p>
     * @param entityCall The callback for entity of insert. (NotNull, PrimaryKeyNullAllowed: when auto-increment)
     * @param optionCall The callback for option of insert. (NoArgAllowed: then no option)
     * @exception EntityAlreadyExistsException When the entity already exists. (unique constraint violation)
     */
    def insert(entityCall: (MbleProductStatus) => Unit)(implicit optionCall: (ScrInsertOption[ProductStatusCB]) => Unit = null): Unit = {
        assertObjectNotNull("entityCall", entityCall);
        doInsert(callbackMbleEntityToDBable(entityCall), callbackInsertOption(optionCall));
    }

    protected def doInsert(et: DbleProductStatus, op: InsertOption[ProductStatusCB]): Unit = {
        assertObjectNotNull("productStatus", et); prepareInsertOption(op); delegateInsert(et, op);
    }

    protected def prepareInsertOption(op: InsertOption[ProductStatusCB]): Unit = {
        if (op == null) { return; }
        assertInsertOptionStatus(op);
        if (op.hasSpecifiedInsertColumn()) {
            op.resolveInsertColumnSpecification(createCBForSpecifiedUpdate());
        }
    }

    @Override
    protected def doCreate(et: Entity, op: InsertOption[_ <: ConditionBean]): Unit = {
        doInsert(downcast(et), downcast(op));
    }

    /**
     * Update the entity modified-only. (ZeroUpdateException, NonExclusiveControl)
     * <pre>
     * DbleProductStatus productStatus = new DbleProductStatus();
     * productStatus.setPK...(value); <span style="color: #3F7E5E">// required</span>
     * productStatus.setFoo...(value); <span style="color: #3F7E5E">// you should set only modified columns</span>
     * <span style="color: #3F7E5E">// you don't need to set values of common columns</span>
     * <span style="color: #3F7E5E">//productStatus.setRegisterUser(value);</span>
     * <span style="color: #3F7E5E">//productStatus.set...;</span>
     * <span style="color: #3F7E5E">// if exclusive control, the value of concurrency column is required</span>
     * productStatus.<span style="color: #DD4747">setVersionNo</span>(value);
     * try {
     *     productStatusBhv.<span style="color: #DD4747">update</span>(productStatus);
     * } catch (EntityAlreadyUpdatedException e) { <span style="color: #3F7E5E">// if concurrent update</span>
     *     ...
     * }
     * </pre>
     * @param entityCall The callback for entity of update. (NotNull, PrimaryKeyNotNull)
     * @param optionCall The callback for option of update. (NoArgAllowed: then no option)
     * @exception EntityAlreadyDeletedException When the entity has already been deleted. (not found)
     * @exception EntityDuplicatedException When the entity has been duplicated.
     * @exception EntityAlreadyExistsException When the entity already exists. (unique constraint violation)
     */
    def update(entityCall: (MbleProductStatus) => Unit)(implicit optionCall: (ScrUpdateOption[ProductStatusCB]) => Unit = null): Unit = {
        assertObjectNotNull("entityCall", entityCall);
        doUpdate(callbackMbleEntityToDBable(entityCall), callbackUpdateOption(optionCall));
    }

    protected def doUpdate(et: DbleProductStatus, op: UpdateOption[ProductStatusCB]): Unit = {
        assertObjectNotNull("productStatus", et); prepareUpdateOption(op); helpUpdateInternally(et, op);
    }

    protected def prepareUpdateOption(op: UpdateOption[ProductStatusCB]): Unit = {
        if (op == null) { return; }
        assertUpdateOptionStatus(op);
        if (op.hasSelfSpecification()) { op.resolveSelfSpecification(createCBForVaryingUpdate()); }
        if (op.hasSpecifiedUpdateColumn()) { op.resolveUpdateColumnSpecification(createCBForSpecifiedUpdate()); }
    }

    protected def createCBForVaryingUpdate(): ProductStatusCB =
    { val cb: ProductStatusCB = newConditionBean(); cb.xsetupForVaryingUpdate(); return cb; }

    protected def createCBForSpecifiedUpdate(): ProductStatusCB =
    { val cb: ProductStatusCB = newConditionBean(); cb.xsetupForSpecifiedUpdate(); return cb; }

    protected def doModify(et: Entity, op: UpdateOption[_ <: ConditionBean]): Unit = { doUpdate(downcast(et), downcast(op)) }

    protected def doModifyNonstrict(et: Entity, op: UpdateOption[_ <: ConditionBean]): Unit =
    { doModify(et, op) }

    /**
     * Insert or update the entity modified-only. (DefaultConstraintsEnabled, NonExclusiveControl) <br />
     * if (the entity has no PK) { insert() } else { update(), but no data, insert() } <br />
     * <p><span style="color: #DD4747; font-size: 120%">Attention, you cannot update by unique keys instead of PK.</span></p>
     * @param entityCall The callback for entity of insert or update. (NotNull, ...depends on insert or update)
     * @param insertOptionCall The callback for option of insert. (NoArgAllowed: then no option)
     * @param updateOptionCall The callback for option of update. (NoArgAllowed: then no option)
     * @exception EntityAlreadyDeletedException When the entity has already been deleted. (not found)
     * @exception EntityDuplicatedException When the entity has been duplicated.
     * @exception EntityAlreadyExistsException When the entity already exists. (unique constraint violation)
     */
    def insertOrUpdate(entityCall: (MbleProductStatus) => Unit)(implicit insertOptionCall: (ScrInsertOption[ProductStatusCB]) => Unit = null, updateOptionCall: (ScrUpdateOption[ProductStatusCB]) => Unit = null): Unit = {
        doInsertOrUpdate(callbackMbleEntityToDBable(entityCall), callbackInsertOption(insertOptionCall), callbackUpdateOption(updateOptionCall));
    }

    protected def doInsertOrUpdate(et: DbleProductStatus, iop: InsertOption[ProductStatusCB], uop: UpdateOption[ProductStatusCB]): Unit = {
        assertObjectNotNull("productStatus", et); helpInsertOrUpdateInternally(et, iop, uop);
    }

    protected def doCreateOrModify(et: Entity, iop: InsertOption[_ <: ConditionBean], uop: UpdateOption[_ <: ConditionBean]): Unit =
    { doInsertOrUpdate(downcast(et), downcast(iop), downcast(uop)) }

    @Override
    protected def doCreateOrModifyNonstrict(et: Entity, iop: InsertOption[_ <: ConditionBean], uop: UpdateOption[_ <: ConditionBean]): Unit = {
        doCreateOrModify(et, iop, uop);
    }

    /**
     * Delete the entity. (ZeroUpdateException, NonExclusiveControl)
     * <pre>
     * DbleProductStatus productStatus = new DbleProductStatus();
     * productStatus.setPK...(value); <span style="color: #3F7E5E">// required</span>
     * <span style="color: #3F7E5E">// if exclusive control, the value of concurrency column is required</span>
     * productStatus.<span style="color: #DD4747">setVersionNo</span>(value);
     * try {
     *     productStatusBhv.<span style="color: #DD4747">delete</span>(productStatus);
     * } catch (EntityAlreadyUpdatedException e) { <span style="color: #3F7E5E">// if concurrent update</span>
     *     ...
     * }
     * </pre>
     * @param entityCall The callback for entity of delete. (NotNull, PrimaryKeyNotNull)
     * @param optionCall The callback for option of delete. (NoArgAllowed: then no option)
     * @exception EntityAlreadyDeletedException When the entity has already been deleted. (not found)
     * @exception EntityDuplicatedException When the entity has been duplicated.
     */
    def delete(entityCall: (MbleProductStatus) => Unit)(implicit optionCall: (ScrDeleteOption[ProductStatusCB]) => Unit = null): Unit = {
        doDelete(callbackMbleEntityToDBable(entityCall), callbackDeleteOption(optionCall));
    }

    protected def doDelete(et: DbleProductStatus, op: DeleteOption[ProductStatusCB]): Unit = {
        assertObjectNotNull("productStatus", et); prepareDeleteOption(op); helpDeleteInternally(et, op);
    }

    protected def prepareDeleteOption(op: DeleteOption[ProductStatusCB]): Unit = { if (op != null) { assertDeleteOptionStatus(op); } }

    protected def doRemove(et: Entity, op: DeleteOption[_ <: ConditionBean]): Unit = { doDelete(downcast(et), downcast(op)) }

    protected def doRemoveNonstrict(et: Entity, op: DeleteOption[_ <: ConditionBean]): Unit =
    { doRemove(et, op) }

    // ===================================================================================
    //                                                                        Batch Update
    //                                                                        ============
    /**
     * Batch-insert the entity list modified-only of same-set columns. (DefaultConstraintsEnabled) <br />
     * This method uses executeBatch() of java.sql.PreparedStatement. <br />
     * <p><span style="color: #DD4747; font-size: 120%">The columns of least common multiple are registered like this:</span></p>
     * <pre>
     * for (... : ...) {
     *     DbleProductStatus productStatus = new DbleProductStatus();
     *     productStatus.setFooName("foo");
     *     if (...) {
     *         productStatus.setFooPrice(123);
     *     }
     *     <span style="color: #3F7E5E">// FOO_NAME and FOO_PRICE (and record meta columns) are registered</span>
     *     <span style="color: #3F7E5E">// FOO_PRICE not-called in any entities are registered as null without default value</span>
     *     <span style="color: #3F7E5E">// columns not-called in all entities are registered as null or default value</span>
     *     productStatusList.add(productStatus);
     * }
     * productStatusBhv.<span style="color: #DD4747">batchInsert</span>(productStatusList);
     * </pre>
     * <p>While, when the entities are created by select, all columns are registered.</p>
     * <p>And if the table has an identity, entities after the process don't have incremented values.
     * (When you use the (normal) insert(), you can get the incremented value from your entity)</p>
     * @param productStatusList The list of the entity. (NotNull, EmptyAllowed, PrimaryKeyNullAllowed: when auto-increment)
     * @return The array of inserted count. (NotNull, EmptyAllowed)
     */
    def batchInsert(batchCall: (ScrBatchEntityList[MbleProductStatus]) => Unit)(implicit optionCall: (ScrInsertOption[ProductStatusCB]) => Unit = null): Array[Int] = {
        return doBatchInsert(callbackBatch(batchCall), callbackInsertOption(optionCall));
    }

    protected def doBatchInsert(ls: List[DbleProductStatus], op: InsertOption[ProductStatusCB]): Array[Int] = {
        assertObjectNotNull("productStatusList", ls);
        val rlop: InsertOption[ProductStatusCB] = if (op != null) { op } else { createPlainInsertOption() }
        prepareBatchInsertOption(ls, op); // required
        return delegateBatchInsert(ls, op);
    }

    protected def prepareBatchInsertOption(ls: List[DbleProductStatus], op: InsertOption[ProductStatusCB]): Unit = {
        op.xallowInsertColumnModifiedPropertiesFragmented();
        op.xacceptInsertColumnModifiedPropertiesIfNeeds(ls);
        prepareInsertOption(op);
    }

    protected def doLumpCreate(ls: List[Entity], op: InsertOption[_ <: ConditionBean]): Array[Int] = { doBatchInsert(downcast(ls), downcast(op)) }

    /**
     * Batch-update the entity list modified-only of same-set columns. (NonExclusiveControl) <br />
     * This method uses executeBatch() of java.sql.PreparedStatement. <br />
     * <span style="color: #DD4747; font-size: 120%">You should specify same-set columns to all entities like this:</span>
     * <pre>
     * for (... : ...) {
     *     DbleProductStatus productStatus = new DbleProductStatus();
     *     productStatus.setFooName("foo");
     *     if (...) {
     *         productStatus.setFooPrice(123);
     *     } else {
     *         productStatus.setFooPrice(null); <span style="color: #3F7E5E">// updated as null</span>
     *         <span style="color: #3F7E5E">//productStatus.setFooDate(...); // *not allowed, fragmented</span>
     *     }
     *     <span style="color: #3F7E5E">// FOO_NAME and FOO_PRICE (and record meta columns) are updated</span>
     *     <span style="color: #3F7E5E">// (others are not updated: their values are kept)</span>
     *     productStatusList.add(productStatus);
     * }
     * productStatusBhv.<span style="color: #DD4747">batchUpdate</span>(productStatusList);
     * </pre>
     * @param productStatusList The list of the entity. (NotNull, EmptyAllowed, PrimaryKeyNotNull)
     * @return The array of updated count. (NotNull, EmptyAllowed)
     * @exception EntityAlreadyDeletedException When the entity has already been deleted. (not found)
     */
    def batchUpdate(batchCall: (ScrBatchEntityList[MbleProductStatus]) => Unit)(implicit optionCall: (ScrUpdateOption[ProductStatusCB]) => Unit = null): Array[Int] = {
        return doBatchUpdate(callbackBatch(batchCall), callbackUpdateOption(optionCall));
    }

    protected def doBatchUpdate(ls: List[DbleProductStatus], op: UpdateOption[ProductStatusCB]): Array[Int] = {
        assertObjectNotNull("productStatusList", ls);
        val rlop: UpdateOption[ProductStatusCB] = if (op != null) { op } else { createPlainUpdateOption() }
        prepareBatchUpdateOption(ls, rlop); // required
        return delegateBatchUpdate(ls, rlop);
    }

    protected def prepareBatchUpdateOption(ls: List[DbleProductStatus], op: UpdateOption[ProductStatusCB]): Unit = {
        op.xacceptUpdateColumnModifiedPropertiesIfNeeds(ls);
        prepareUpdateOption(op);
    }

    protected def doLumpModify(ls: List[Entity], op: UpdateOption[_ <: ConditionBean]): Array[Int] = { doBatchUpdate(downcast(ls), downcast(op)) }

    protected def doLumpModifyNonstrict(ls: List[Entity], op: UpdateOption[_ <: ConditionBean]): Array[Int] =
    { doLumpModify(ls, op) }

    /**
     * Batch-delete the entity list. (NonExclusiveControl) <br />
     * This method uses executeBatch() of java.sql.PreparedStatement.
     * @param productStatusList The list of the entity. (NotNull, EmptyAllowed, PrimaryKeyNotNull)
     * @return The array of deleted count. (NotNull, EmptyAllowed)
     * @exception EntityAlreadyDeletedException When the entity has already been deleted. (not found)
     */
    def batchDelete(batchCall: (ScrBatchEntityList[MbleProductStatus]) => Unit)(implicit optionCall: (ScrDeleteOption[ProductStatusCB]) => Unit = null): Array[Int] = {
        return doBatchDelete(callbackBatch(batchCall), callbackDeleteOption(optionCall));
    }

    protected def doBatchDelete(ls: List[DbleProductStatus], op: DeleteOption[ProductStatusCB]): Array[Int] = {
        assertObjectNotNull("productStatusList", ls); prepareDeleteOption(op);
        return delegateBatchDelete(ls, op);
    }

    protected def doLumpRemove(ls: List[Entity], op: DeleteOption[_ <: ConditionBean]): Array[Int] = { doBatchDelete(downcast(ls), downcast(op)) }

    protected def doLumpRemoveNonstrict(ls: List[Entity], op: DeleteOption[_ <: ConditionBean]): Array[Int] =
    { return doLumpRemove(ls, op); }

    // ===================================================================================
    //                                                                        Query Update
    //                                                                        ============
    /**
     * Insert the several entities by query (modified-only for fixed value).
     * <pre>
     * productStatusBhv.<span style="color: #DD4747">queryInsert</span>(new QueryInsertSetupper&lt;DbleProductStatus, ProductStatusCB&gt;() {
     *     public ConditionBean setup(productStatus entity, ProductStatusCB intoCB) {
     *         FooCB cb = FooCB();
     *         cb.setupSelect_Bar();
     *
     *         <span style="color: #3F7E5E">// mapping</span>
     *         intoCB.specify().columnMyName().mappedFrom(cb.specify().columnFooName());
     *         intoCB.specify().columnMyCount().mappedFrom(cb.specify().columnFooCount());
     *         intoCB.specify().columnMyDate().mappedFrom(cb.specify().specifyBar().columnBarDate());
     *         entity.setMyFixedValue("foo"); <span style="color: #3F7E5E">// fixed value</span>
     *         <span style="color: #3F7E5E">// you don't need to set values of common columns</span>
     *         <span style="color: #3F7E5E">//entity.setRegisterUser(value);</span>
     *         <span style="color: #3F7E5E">//entity.set...;</span>
     *         <span style="color: #3F7E5E">// you don't need to set a value of concurrency column</span>
     *         <span style="color: #3F7E5E">//entity.setVersionNo(value);</span>
     *
     *         return cb;
     *     }
     * });
     * </pre>
     * @param setupper The setup-per of query-insert. (NotNull)
     * @return The inserted count.
     */
    def queryInsert(setupper: QueryInsertSetupper[DbleProductStatus, ProductStatusCB]): Int = {
        return doQueryInsert(setupper, null);
    }

    protected def doQueryInsert(sp: QueryInsertSetupper[DbleProductStatus, ProductStatusCB], op: InsertOption[ProductStatusCB]): Int = {
        assertObjectNotNull("setupper", sp); prepareInsertOption(op);
        val et: DbleProductStatus = newEntity();
        val cb: ProductStatusCB = createCBForQueryInsert();
        return delegateQueryInsert(et, cb, sp.setup(et, cb), op);
    }

    protected def createCBForQueryInsert(): ProductStatusCB =
    { val cb: ProductStatusCB = newConditionBean(); cb.xsetupForQueryInsert(); return cb; }

    protected def doRangeCreate(setupper: QueryInsertSetupper[_ <: Entity, _ <: ConditionBean], option: InsertOption[_ <: ConditionBean]): Int =
    { doQueryInsert(downcast(setupper), downcast(option)) }

    /**
     * Update the several entities by query non-strictly modified-only. (NonExclusiveControl)
     * <pre>
     * DbleProductStatus productStatus = new DbleProductStatus();
     * <span style="color: #3F7E5E">// you don't need to set PK value</span>
     * <span style="color: #3F7E5E">//productStatus.setPK...(value);</span>
     * productStatus.setFoo...(value); <span style="color: #3F7E5E">// you should set only modified columns</span>
     * <span style="color: #3F7E5E">// you don't need to set values of common columns</span>
     * <span style="color: #3F7E5E">//productStatus.setRegisterUser(value);</span>
     * <span style="color: #3F7E5E">//productStatus.set...;</span>
     * <span style="color: #3F7E5E">// you don't need to set a value of concurrency column</span>
     * <span style="color: #3F7E5E">// (auto-increment for version number is valid though non-exclusive control)</span>
     * <span style="color: #3F7E5E">//productStatus.setVersionNo(value);</span>
     * ProductStatusCB cb = new ProductStatusCB();
     * cb.query().setFoo...(value);
     * productStatusBhv.<span style="color: #DD4747">queryUpdate</span>(productStatus, cb);
     * </pre>
     * @param entityCall The callback for entity that contains update values. (NotNull)
     * @param cbCall The callback for condition-bean of ProductStatus. (NotNull)
     * @return The updated count.
     * @exception NonQueryUpdateNotAllowedException When the query has no condition.
     */
    def queryUpdate(entityCall: (MbleProductStatus) => Unit)(cbCall: (ProductStatusCB) => Unit): Int = {
        assertObjectNotNull("entityCall", entityCall); assertObjectNotNull("cbCall", cbCall);
        return doQueryUpdate(callbackMbleEntityToDBable(entityCall), callbackCB(cbCall), null);
    }

    protected def doQueryUpdate(productStatus: DbleProductStatus, cb: ProductStatusCB, op: UpdateOption[ProductStatusCB]): Int = {
        assertObjectNotNull("productStatus", productStatus); assertCBStateValid(cb); prepareUpdateOption(op);
        return if (checkCountBeforeQueryUpdateIfNeeds(cb)) { delegateQueryUpdate(productStatus, cb, op) } else { 0 };
    }

    protected def doRangeModify(et: Entity, cb: ConditionBean, op: UpdateOption[_ <: ConditionBean]): Int =
    { doQueryUpdate(downcast(et), downcast(cb), downcast(op)) }

    /**
     * Delete the several entities by query. (NonExclusiveControl)
     * <pre>
     * ProductStatusCB cb = new ProductStatusCB();
     * cb.query().setFoo...(value);
     * productStatusBhv.<span style="color: #DD4747">queryDelete</span>(productStatus, cb);
     * </pre>
     * @param cbCall The callback for condition-bean of ProductStatus. (NotNull)
     * @return The deleted count.
     * @exception NonQueryDeleteNotAllowedException When the query has no condition.
     */
    def queryDelete(cbCall: (ProductStatusCB) => Unit): Int = {
        assertObjectNotNull("cbCall", cbCall);
        return doQueryDelete(callbackCB(cbCall), null);
    }

    protected def doQueryDelete(cb: ProductStatusCB, op: DeleteOption[ProductStatusCB]): Int = {
        assertCBStateValid(cb); prepareDeleteOption(op);
        return if (checkCountBeforeQueryUpdateIfNeeds(cb)) { delegateQueryDelete(cb, op) } else { 0 };
    }

    protected def doRangeRemove(cb: ConditionBean, op: DeleteOption[_ <: ConditionBean]): Int = { doQueryDelete(downcast(cb), downcast(op)) }

    // ===================================================================================
    //                                                                          OutsideSql
    //                                                                          ==========
    /**
     * Prepare the basic executor of outside-SQL to execute it. <br />
     * The invoker of behavior command should be not null when you call this method.
     * <pre>
     * You can use the methods for outside-SQL are as follows:
     * {Basic}
     *   o selectList()
     *   o execute()
     *   o call()
     *
     * {Entity}
     *   o entityHandling().selectEntity()
     *   o entityHandling().selectEntityWithDeletedCheck()
     *
     * {Paging}
     *   o autoPaging().selectList()
     *   o autoPaging().selectPage()
     *   o manualPaging().selectList()
     *   o manualPaging().selectPage()
     *
     * {Cursor}
     *   o cursorHandling().selectCursor()
     *
     * {Option}
     *   o dynamicBinding().selectList()
     *   o removeBlockComment().selectList()
     *   o removeLineComment().selectList()
     *   o formatSql().selectList()
     * </pre>
     * @return The basic executor of outside-SQL. (NotNull)
     */
    def outsideSql(): ScrOutsideSqlBasicExecutor[ProductStatusBhv] = {
        return toImmutableOutsideSqlBasicExecutor(doOutsideSql());
    }

    protected def toImmutableOutsideSqlBasicExecutor(executor: OutsideSqlBasicExecutor[ProductStatusBhv]): ScrOutsideSqlBasicExecutor[ProductStatusBhv] =
    { new ScrOutsideSqlBasicExecutor(executor) }

    // ===================================================================================
    //                                                                       Assist Helper
    //                                                                       =============
    protected def callbackCB(cbCall: (ProductStatusCB) => Unit): ProductStatusCB = {
        assertObjectNotNull("cbCall", cbCall);
        val cb = newConditionBean(); cbCall(cb); return cb;
    }

    protected def callbackBatch(batchCall: (ScrBatchEntityList[MbleProductStatus]) => Unit): List[DbleProductStatus] = {
        assertObjectNotNull("batchCall", batchCall);
        val batch = new ScrBatchEntityList[MbleProductStatus]();
        val entityList: List[DbleProductStatus] = new ArrayList[DbleProductStatus]();
        batch.entityCallList.asScala.map { entityCall =>
            val entity = newMbleEntity(); entityCall(entity); entity.toDBable;
        }
        return entityList;
    }

    protected def callbackMbleEntity(entityCall: (MbleProductStatus) => Unit): MbleProductStatus = {
        assertObjectNotNull("entityCall", entityCall);
        val entity = newMbleEntity(); entityCall(entity); return entity;
    }

    protected def callbackMbleEntityToDBable(entityCall: (MbleProductStatus) => Unit): DbleProductStatus = {
        return callbackMbleEntity(entityCall).toDBable;
    }

    protected def callbackInsertOption(optionCall: (ScrInsertOption[ProductStatusCB]) => Unit): InsertOption[ProductStatusCB] = {
        if (optionCall == null) { return null; }
        val option = new ScrInsertOption[ProductStatusCB](new InsertOption[ProductStatusCB]());
        optionCall(option); return option.toNative;
    }

    protected def callbackUpdateOption(optionCall: (ScrUpdateOption[ProductStatusCB]) => Unit): UpdateOption[ProductStatusCB] = {
        if (optionCall == null) { return null; }
        val option = new ScrUpdateOption[ProductStatusCB](new UpdateOption[ProductStatusCB]()); optionCall(option); return option.toNative;
    }

    protected def callbackDeleteOption(optionCall: (ScrDeleteOption[ProductStatusCB]) => Unit): DeleteOption[ProductStatusCB] = {
        if (optionCall == null) { return null; }
        val option = new ScrDeleteOption[ProductStatusCB](new DeleteOption[ProductStatusCB]()); optionCall(option); return option.toNative;
    }

    protected def callbackLoader(dbleList: List[DbleProductStatus], loaderCall: (LoaderOfProductStatus) => Unit = null): Unit = {
        if (loaderCall == null) { return; }
        val loader = new LoaderOfProductStatus();
        loader.ready(dbleList.asInstanceOf[List[DbleProductStatus]], _behaviorSelector);
        loaderCall(loader);
    }

    protected def newMbleEntity(): MbleProductStatus = { new MbleProductStatus() }
    protected def newPagingView(rb: PagingResultBean[DbleProductStatus]): ScrPagingView[ProductStatus] =
    { new ScrPagingView(toImmutableEntityList(rb), rb) }

    protected def typeOfSelectedEntity(): Class[DbleProductStatus] = { classOf[DbleProductStatus] }
    protected def downcast(et: Entity): DbleProductStatus = { helpEntityDowncastInternally(et, classOf[DbleProductStatus]) }
    protected def downcast(cb: ConditionBean): ProductStatusCB = { helpConditionBeanDowncastInternally(cb, classOf[ProductStatusCB]) }
    protected def downcast(ls: List[_ <: Entity]): List[DbleProductStatus] = { ls.asInstanceOf[List[DbleProductStatus]] }
    protected def downcast(op: InsertOption[_ <: ConditionBean]): InsertOption[ProductStatusCB] = { op.asInstanceOf[InsertOption[ProductStatusCB]] }
    protected def downcast(op: UpdateOption[_ <: ConditionBean]): UpdateOption[ProductStatusCB] = { op.asInstanceOf[UpdateOption[ProductStatusCB]] }
    protected def downcast(op: DeleteOption[_ <: ConditionBean]): DeleteOption[ProductStatusCB] = { op.asInstanceOf[DeleteOption[ProductStatusCB]] }
    protected def downcast(sp: QueryInsertSetupper[_ <: Entity, _ <: ConditionBean]): QueryInsertSetupper[DbleProductStatus, ProductStatusCB] =
    { sp.asInstanceOf[QueryInsertSetupper[DbleProductStatus, ProductStatusCB]] }

    // ===================================================================================
    //                                                                        Scala Helper
    //                                                                        ============
    protected def toScalaList[ENTITY](javaList: Collection[ENTITY]): immutable.List[ENTITY] = {
        if (javaList == null) { immutable.List() }
        return immutable.List.fromArray(javaList.toArray()).asInstanceOf[immutable.List[ENTITY]];
    }

    def toImmutableEntityList(dbleList: Collection[DbleProductStatus]): immutable.List[ProductStatus] =
    { toScalaList(dbleList).map(new ProductStatus(_)) }

    def toDBableEntityList(immuList: immutable.List[ProductStatus]): List[DbleProductStatus] =
    { immuList.map(new DbleProductStatus().acceptImmutable(_)).asJava }
}
