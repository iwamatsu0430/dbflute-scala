package com.example.dbflute.scala.dbflute.allcommon;

import scala.collection.immutable;

import java.util.Collection;
import java.util.List;
import java.util.ArrayList;

import org.seasar.dbflute.cbean.ListResultBean;
import org.seasar.dbflute.cbean.PagingBean;
import org.seasar.dbflute.cbean.PagingResultBean;
import org.seasar.dbflute.immutable.DBableEntity;
import org.seasar.dbflute.immutable.outsidesql._;
import org.seasar.dbflute.outsidesql._;
import org.seasar.dbflute.outsidesql.executor._;
import org.seasar.dbflute.outsidesql.typed._;
import org.seasar.dbflute.jdbc.CursorHandler;
import org.seasar.dbflute.jdbc.StatementConfig;

/* _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                                                                      _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                                                                      _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                  OutsideSql                                          _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                                                                      _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                                   Basic Executor                     _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                                                                      _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                                                                      _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/ */

/**
 * The executor of outside-SQL.
 * <pre>
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
 *   o removeBlockComment().selectList()
 *   o removeLineComment().selectList()
 *   o formatSql().selectList()
 *   o configure(...).selectList()
 * </pre>
 * @param [BEHAVIOR] The type of behavior.
 * @author DBFlute(AutoGenerator)
 */
class ScrOutsideSqlFreeStyleBasicExecutor[BEHAVIOR](executor: OutsideSqlBasicExecutor[BEHAVIOR]) {

    // ===================================================================================
    //                                                                              Entity
    //                                                                              ======
    /**
     * Select entity by the outside-SQL. {FreeStyle Interface}<br />
     * This method can accept each element: path, parameter-bean(Object type), entity-type.
     * <pre>
     * String path = MemberBhv.PATH_selectSimpleMember;
     * SimpleMemberPmb pmb = new SimpleMemberPmb();
     * pmb.setMemberId(3);
     * Class&lt;SimpleMember&gt; entityType = SimpleMember.class;
     * SimpleMember member
     *     = memberBhv.outsideSql().entityHandling().<span style="color: #DD4747">selectEntity</span>(path, pmb, entityType);
     * if (member != null) {
     *     ... = member.get...();
     * } else {
     *     ...
     * }
     * </pre>
     * @param <ENTITY> The type of entity.
     * @param path The path of SQL file. (NotNull)
     * @param pmb The object as parameter-bean. Allowed types are Bean object and Map object. (NullAllowed)
     * @param entityType The type of entity. (NotNull)
     * @return The selected entity. (NullAllowed)
     * @exception org.seasar.dbflute.exception.OutsideSqlNotFoundException When the outside-SQL is not found.
     * @exception org.seasar.dbflute.exception.EntityDuplicatedException When the entity is duplicated.
     */
    def selectEntity[ENTITY](path: String, pmb: Object, entityType: Class[ENTITY]): Option[ENTITY] = {
        return Option(executor.entityHandling().selectEntity(path, pmb, entityType));
    }

    // ===================================================================================
    //                                                                              Select
    //                                                                              ======
    /**
     * Select the list of the entity by the outsideSql. {FreeStyle Interface}<br />
     * This method can accept each element: path, parameter-bean(Object type), entity-type.
     * <pre>
     * String path = MemberBhv.PATH_selectSimpleMember;
     * SimpleMemberPmb pmb = new SimpleMemberPmb();
     * pmb.setMemberName_PrefixSearch("S");
     * Class&lt;SimpleMember&gt; entityType = SimpleMember.class;
     * ListResultBean&lt;SimpleMember&gt; memberList
     *     = memberBhv.outsideSql().<span style="color: #DD4747">selectList</span>(path, pmb, entityType);
     * for (SimpleMember member : memberList) {
     *     ... = member.get...();
     * }
     * </pre>
     * It needs to use customize-entity and parameter-bean.
     * The way to generate them is following:
     * <pre>
     * -- #df:entity#
     * -- !df:pmb!
     * -- !!Integer memberId!!
     * -- !!String memberName!!
     * -- !!...!!
     * </pre>
     * @param <ENTITY> The type of entity for element.
     * @param path The path of SQL file. (NotNull)
     * @param pmb The object as parameter-bean. Allowed types are Bean object and Map object. (NullAllowed)
     * @param entityType The element type of entity. (NotNull)
     * @return The result bean of selected list. (NotNull)
     * @exception org.seasar.dbflute.exception.OutsideSqlNotFoundException When the outsideSql is not found.
     * @exception org.seasar.dbflute.exception.DangerousResultSizeException When the result size is over the specified safety size.
     */
    def selectList[ENTITY](path: String, pmb: Object, entityType: Class[ENTITY]): ListResultBean[ENTITY] = {
        return executor.selectList(path, pmb, entityType);
    }

    // ===================================================================================
    //                                                                             Execute
    //                                                                             =======
    /**
     * Execute the outsideSql. (insert, update, delete, etc...) {FreeStyle Interface}<br />
     * This method can accept each element: path, parameter-bean(Object type).
     * <pre>
     * String path = MemberBhv.PATH_selectSimpleMember;
     * SimpleMemberPmb pmb = new SimpleMemberPmb();
     * pmb.setMemberId(3);
     * int count = memberBhv.outsideSql().<span style="color: #DD4747">execute</span>(path, pmb);
     * </pre>
     * @param path The path of SQL file. (NotNull)
     * @param pmb The parameter-bean. Allowed types are Bean object and Map object. (NullAllowed)
     * @return The count of execution.
     * @exception org.seasar.dbflute.exception.OutsideSqlNotFoundException When the outsideSql is not found.
     */
    def execute(path: String, pmb: Object): Int = {
        return executor.execute(path, pmb);
    }

    // ===================================================================================
    //                                                                              Paging
    //                                                                              ======
    /**
     * Select page by the outside-SQL. {FreeStyle Interface}<br />
     * (both count-select and paging-select are executed)<br />
     * This method can accept each element: path, parameter-bean(Object type), entity-type.
     * <pre>
     * String path = MemberBhv.PATH_selectSimpleMember;
     * SimpleMemberPmb pmb = new SimpleMemberPmb();
     * pmb.setMemberName_PrefixSearch("S");
     * pmb.paging(20, 3); <span style="color: #3F7E5E">// 20 records per a page and current page number is 3</span>
     * Class&lt;SimpleMember&gt; entityType = SimpleMember.class;
     * PagingResultBean&lt;SimpleMember&gt; page
     *     = memberBhv.outsideSql().manualPaging().<span style="color: #DD4747">selectPage</span>(path, pmb, entityType);
     * int allRecordCount = page.getAllRecordCount();
     * int allPageCount = page.getAllPageCount();
     * boolean isExistPrePage = page.isExistPrePage();
     * boolean isExistNextPage = page.isExistNextPage();
     * ...
     * for (SimpleMember member : page) {
     *     ... = member.get...();
     * }
     * </pre>
     * The parameter-bean needs to extend SimplePagingBean.
     * The way to generate it is following:
     * <pre>
     * <span style="color: #3F7E5E">-- !df:pmb extends Paging!</span>
     * <span style="color: #3F7E5E">-- !!Integer memberId!!</span>
     * <span style="color: #3F7E5E">-- !!...!!</span>
     * </pre>
     * You can realize by pagingBean's isPaging() method on your 'Parameter Comment'.
     * It returns false when it executes Count. And it returns true when it executes Paging.
     * <pre>
     * e.g. ManualPaging and MySQL
     * <span style="color: #3F7E5E">/*IF pmb.isPaging()*/</span>
     * select member.MEMBER_ID
     *      , member.MEMBER_NAME
     *      , memberStatus.MEMBER_STATUS_NAME
     * <span style="color: #3F7E5E">-- ELSE select count(*)</span>
     * <span style="color: #3F7E5E">/*END*/</span>
     *   from MEMBER member
     *     <span style="color: #3F7E5E">/*IF pmb.isPaging()*/</span>
     *     left outer join MEMBER_STATUS memberStatus
     *       on member.MEMBER_STATUS_CODE = memberStatus.MEMBER_STATUS_CODE
     *     <span style="color: #3F7E5E">/*END*/</span>
     *  <span style="color: #3F7E5E">/*BEGIN*/</span>
     *  where
     *    <span style="color: #3F7E5E">/*IF pmb.memberId != null*/</span>
     *    member.MEMBER_ID = <span style="color: #3F7E5E">/*pmb.memberId*/</span>'123'
     *    <span style="color: #3F7E5E">/*END*/</span>
     *    <span style="color: #3F7E5E">/*IF pmb.memberName != null*/</span>
     *    and member.MEMBER_NAME like <span style="color: #3F7E5E">/*pmb.memberName*/</span>'Billy%'
     *    <span style="color: #3F7E5E">/*END*/</span>
     *  <span style="color: #3F7E5E">/*END*/</span>
     *  <span style="color: #3F7E5E">/*IF pmb.isPaging()*/</span>
     *  order by member.UPDATE_DATETIME desc
     *  <span style="color: #3F7E5E">/*END*/</span>
     *  <span style="color: #3F7E5E">/*IF pmb.isPaging()*/</span>
     *  limit <span style="color: #3F7E5E">/*$pmb.pageStartIndex*/</span>80, <span style="color: #3F7E5E">/*$pmb.fetchSize*/</span>20
     *  <span style="color: #3F7E5E">/*END*/</span>
     * </pre>
     * @param <ENTITY> The type of entity.
     * @param path The path of SQL that executes count and paging. (NotNull)
     * @param pmb The bean of paging parameter. (NotNull)
     * @param entityType The type of result entity. (NotNull)
     * @return The result bean of paging. (NotNull)
     * @exception org.seasar.dbflute.exception.OutsideSqlNotFoundException When the outside-SQL is not found.
     * @exception org.seasar.dbflute.exception.DangerousResultSizeException When the result size is over the specified safety size.
     */
    def selectPage[ENTITY](path: String, pmb: PagingBean, entityType: Class[ENTITY]): ScrPagingView[ENTITY] = {
        val page = executor.manualPaging.selectPage(path, pmb, entityType);
        return createPagingView[ENTITY](toScalaList(page), page);
    }

    /**
     * Select list with paging by the outside-SQL. {FreeStyle Interface}<br />
     * (count-select is not executed, only paging-select)<br />
     * This method can accept each element: path, parameter-bean(Object type), entity-type.
     * <pre>
     * String path = MemberBhv.PATH_selectSimpleMember;
     * SimpleMemberPmb pmb = new SimpleMemberPmb();
     * pmb.setMemberName_PrefixSearch("S");
     * pmb.paging(20, 3); <span style="color: #3F7E5E">// 20 records per a page and current page number is 3</span>
     * Class&lt;SimpleMember&gt; entityType = SimpleMember.class;
     * ListResultBean&lt;SimpleMember&gt; memberList
     *     = memberBhv.outsideSql().manualPaging().<span style="color: #DD4747">selectList</span>(path, pmb, entityType);
     * for (SimpleMember member : memberList) {
     *     ... = member.get...();
     * }
     * </pre>
     * The parameter-bean needs to extend SimplePagingBean.
     * The way to generate it is following:
     * <pre>
     * <span style="color: #3F7E5E">-- !df:pmb extends Paging!</span>
     * <span style="color: #3F7E5E">-- !!Integer memberId!!</span>
     * <span style="color: #3F7E5E">-- !!...!!</span>
     * </pre>
     * You don't need to use pagingBean's isPaging() method on your 'Parameter Comment'.
     * <pre>
     * e.g. ManualPaging and MySQL 
     * select member.MEMBER_ID
     *      , member.MEMBER_NAME
     *      , memberStatus.MEMBER_STATUS_NAME
     *   from MEMBER member
     *     left outer join MEMBER_STATUS memberStatus
     *       on member.MEMBER_STATUS_CODE = memberStatus.MEMBER_STATUS_CODE
     *  <span style="color: #3F7E5E">/*BEGIN*/</span>
     *  where
     *    <span style="color: #3F7E5E">/*IF pmb.memberId != null*/</span>
     *    member.MEMBER_ID = <span style="color: #3F7E5E">/*pmb.memberId*/</span>'123'
     *    <span style="color: #3F7E5E">/*END*/</span>
     *    <span style="color: #3F7E5E">/*IF pmb.memberName != null*/</span>
     *    and member.MEMBER_NAME like <span style="color: #3F7E5E">/*pmb.memberName*/</span>'Billy%'
     *    <span style="color: #3F7E5E">/*END*/</span>
     *  <span style="color: #3F7E5E">/*END*/</span>
     *  order by member.UPDATE_DATETIME desc
     *  limit <span style="color: #3F7E5E">/*$pmb.pageStartIndex*/</span>80, <span style="color: #3F7E5E">/*$pmb.fetchSize*/</span>20
     * </pre>
     * @param <ENTITY> The type of entity.
     * @param path The path of SQL that executes count and paging. (NotNull)
     * @param pmb The bean of paging parameter. (NotNull)
     * @param entityType The type of result entity. (NotNull)
     * @return The result bean of paged list. (NotNull)
     * @exception org.seasar.dbflute.exception.OutsideSqlNotFoundException When the outside-SQL is not found.
     * @exception org.seasar.dbflute.exception.DangerousResultSizeException When the result size is over the specified safety size.
     */
    def selectPagingListOnly[ENTITY](path: String, pmb: PagingBean, entityType: Class[ENTITY]): immutable.List[ENTITY] = {
        return toScalaList(executor.selectList(path, pmb, entityType));
    }

    /**
     * Prepare paging by cursor-skip for free-style.
     * <pre>
     * memberBhv.outsideSql().freeStyle().<span style="color: #DD4747">pagingByCursorSkip()</span>.selectEntity(pmb);
     * </pre>
     * @return The executor of outsideSql. (NotNull)
     */
    def pagingByCursorSkip(): ScrOutsideSqlFreeStyleAutoPagingExecutor[BEHAVIOR] = {
        return createOutsideSqlFreeStyleAutoPagingExecutor();
    }

    protected def createOutsideSqlFreeStyleAutoPagingExecutor(): ScrOutsideSqlFreeStyleAutoPagingExecutor[BEHAVIOR] = {
        return new ScrOutsideSqlFreeStyleAutoPagingExecutor[BEHAVIOR](executor.autoPaging());
    }

    // ===================================================================================
    //                                                                              Cursor
    //                                                                              ======
    /**
     * Select the cursor of the entity by outside-SQL. {FreeStyle Interface}<br />
     * This method can accept each element: path, parameter-bean(Object type), cursor-handler.
     * <pre>
     * String path = MemberBhv.PATH_selectSimpleMember;
     * SimpleMemberPmb pmb = new SimpleMemberPmb();
     * pmb.setMemberName_PrefixSearch("S");
     * memberBhv.outsideSql().cursorHandling()
     *         .<span style="color: #DD4747">selectCursor</span>(path, pmb, new PurchaseSummaryMemberCursorHandler() {
     *     protected Object fetchCursor(PurchaseSummaryMemberCursor cursor) throws SQLException {
     *         while (cursor.next()) {
     *             Integer memberId = cursor.getMemberId();
     *             String memberName = cursor.getMemberName();
     *             ...
     *         }
     *         return null;
     *     }
     * });
     * </pre>
     * It needs to use type-safe-cursor instead of customize-entity.
     * The way to generate it is following:
     * <pre>
     * <span style="color: #3F7E5E">-- #df:entity#</span>
     * <span style="color: #3F7E5E">-- +cursor+</span>
     * </pre>
     * @param path The path of SQL file. (NotNull)
     * @param pmb The object as parameter-bean. Allowed types are Bean object and Map object. (NullAllowed)
     * @param handler The handler of cursor called back with result set. (NotNull)
     * @return The result object that the cursor handler returns. (NullAllowed)
     * @exception org.seasar.dbflute.exception.OutsideSqlNotFoundException When the outside-SQL is not found.
     */
    def selectCursor(path: String, pmb: Object)(handler: CursorHandler): Object = {
        return executor.cursorHandling().selectCursor(path, pmb, handler);
    }

    // ===================================================================================
    //                                                                              Option
    //                                                                              ======
    // -----------------------------------------------------
    //                                       Remove from SQL
    //                                       ---------------
    /**
     * Set up remove-block-comment for this outsideSql.
     * @return this. (NotNull)
     */
    def removeBlockComment(): ScrOutsideSqlFreeStyleBasicExecutor[BEHAVIOR] = {
        executor.removeBlockComment(); return this;
    }

    /**
     * Set up remove-line-comment for this outsideSql.
     * @return this. (NotNull)
     */
    def removeLineComment(): ScrOutsideSqlFreeStyleBasicExecutor[BEHAVIOR] = {
        executor.removeLineComment(); return this;
    }

    // -----------------------------------------------------
    //                                            Format SQL
    //                                            ----------
    /**
     * Set up format-SQL for this outsideSql. <br />
     * (For example, empty lines removed)
     * @return this. (NotNull)
     */
    def formatSql(): ScrOutsideSqlFreeStyleBasicExecutor[BEHAVIOR] = {
        executor.formatSql(); return this;
    }

    // -----------------------------------------------------
    //                                       StatementConfig
    //                                       ---------------
    /**
     * Configure statement JDBC options. (For example, queryTimeout, fetchSize, ...)
     * @param statementConfig The configuration of statement. (NullAllowed)
     * @return this. (NotNull)
     */
    def configure(statementConfig: StatementConfig): ScrOutsideSqlFreeStyleBasicExecutor[BEHAVIOR] = {
        executor.configure(statementConfig); return this;
    }

    // ===================================================================================
    //                                                                        Scala Helper
    //                                                                        ============
    protected def toScalaList[ENTITY](javaList: Collection[ENTITY]): immutable.List[ENTITY] = {
        if (javaList == null) { scala.collection.immutable.List() }
        return immutable.List.fromArray(javaList.toArray()).asInstanceOf[immutable.List[ENTITY]];
    }

    protected def createPagingView[ENTITY](selectedList: immutable.List[ENTITY], bean: PagingResultBean[_]): ScrPagingView[ENTITY] = {
        return new ScrPagingView[ENTITY](selectedList, bean);
    }
}

/* _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                                                                      _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                                                                      _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                  OutsideSql                                          _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                                                                      _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                               AutoPaging Executor                    _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                                                                      _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/                                                                      _/_/_/_/_/_/_/_/_/_/_/ */
/* _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/ */

class ScrOutsideSqlFreeStyleAutoPagingExecutor[BEHAVIOR](executor: OutsideSqlAutoPagingExecutor[BEHAVIOR]) {

    // ===================================================================================
    //                                                                                Page
    //                                                                                ====
    /**
     * Select page by the outside-SQL. {FreeStyle Interface}<br />
     * (both count-select and paging-select are executed)<br />
     * This method can accept each element: path, parameter-bean(Object type), entity-type.
     * <pre>
     * String path = MemberBhv.PATH_selectSimpleMember;
     * SimpleMemberPmb pmb = new SimpleMemberPmb();
     * pmb.setMemberName_PrefixSearch("S");
     * pmb.paging(20, 3); <span style="color: #3F7E5E">// 20 records per a page and current page number is 3</span>
     * Class&lt;SimpleMember&gt; entityType = SimpleMember.class;
     * PagingResultBean&lt;SimpleMember&gt; page
     *     = memberBhv.outsideSql().freeStyle().pagingByCursorSkip().<span style="color: #DD4747">selectPage</span>(path, pmb, entityType);
     * int allRecordCount = page.getAllRecordCount();
     * int allPageCount = page.getAllPageCount();
     * boolean isExistPrePage = page.isExistPrePage();
     * boolean isExistNextPage = page.isExistNextPage();
     * ...
     * for (SimpleMember member : page) {
     *     ... = member.get...();
     * }
     * </pre>
     * The parameter-bean needs to extend SimplePagingBean.
     * The way to generate it is following:
     * <pre>
     * <span style="color: #3F7E5E">-- !df:pmb extends Paging!</span>
     * <span style="color: #3F7E5E">-- !!Integer memberId!!</span>
     * <span style="color: #3F7E5E">-- !!...!!</span>
     * </pre>
     * You can realize by pagingBean's isPaging() method on your 'Parameter Comment'.
     * It returns false when it executes Count. And it returns true when it executes Paging.
     * <pre>
     * e.g. ManualPaging and MySQL
     * <span style="color: #3F7E5E">/*IF pmb.isPaging()*/</span>
     * select member.MEMBER_ID
     *      , member.MEMBER_NAME
     *      , memberStatus.MEMBER_STATUS_NAME
     * <span style="color: #3F7E5E">-- ELSE select count(*)</span>
     * <span style="color: #3F7E5E">/*END*/</span>
     *   from MEMBER member
     *     <span style="color: #3F7E5E">/*IF pmb.isPaging()*/</span>
     *     left outer join MEMBER_STATUS memberStatus
     *       on member.MEMBER_STATUS_CODE = memberStatus.MEMBER_STATUS_CODE
     *     <span style="color: #3F7E5E">/*END*/</span>
     *  <span style="color: #3F7E5E">/*BEGIN*/</span>
     *  where
     *    <span style="color: #3F7E5E">/*IF pmb.memberId != null*/</span>
     *    member.MEMBER_ID = <span style="color: #3F7E5E">/*pmb.memberId*/</span>'123'
     *    <span style="color: #3F7E5E">/*END*/</span>
     *    <span style="color: #3F7E5E">/*IF pmb.memberName != null*/</span>
     *    and member.MEMBER_NAME like <span style="color: #3F7E5E">/*pmb.memberName*/</span>'Billy%'
     *    <span style="color: #3F7E5E">/*END*/</span>
     *  <span style="color: #3F7E5E">/*END*/</span>
     *  <span style="color: #3F7E5E">/*IF pmb.isPaging()*/</span>
     *  order by member.UPDATE_DATETIME desc
     *  <span style="color: #3F7E5E">/*END*/</span>
     *  <span style="color: #3F7E5E">/*IF pmb.isPaging()*/</span>
     *  limit <span style="color: #3F7E5E">/*$pmb.pageStartIndex*/</span>80, <span style="color: #3F7E5E">/*$pmb.fetchSize*/</span>20
     *  <span style="color: #3F7E5E">/*END*/</span>
     * </pre>
     * @param <ENTITY> The type of entity.
     * @param path The path of SQL that executes count and paging. (NotNull)
     * @param pmb The bean of paging parameter. (NotNull)
     * @param entityType The type of result entity. (NotNull)
     * @return The result bean of paging. (NotNull)
     * @exception org.seasar.dbflute.exception.OutsideSqlNotFoundException When the outside-SQL is not found.
     * @exception org.seasar.dbflute.exception.DangerousResultSizeException When the result size is over the specified safety size.
     */
    def selectPage[ENTITY](path: String, pmb: PagingBean, entityType: Class[ENTITY]): ScrPagingView[ENTITY] = {
        val page = executor.selectPage(path, pmb, entityType);
        return createPagingView[ENTITY](toScalaList(page), page);
    }

    // ===================================================================================
    //                                                                                List
    //                                                                                ====
    /**
     * Select list only of paging by the outside-SQL. {FreeStyle Interface}<br />
     * (count-select is not executed, only paging-select)<br />
     * This method can accept each element: path, parameter-bean(Object type), entity-type.
     * <pre>
     * String path = MemberBhv.PATH_selectSimpleMember;
     * SimpleMemberPmb pmb = new SimpleMemberPmb();
     * pmb.setMemberName_PrefixSearch("S");
     * pmb.paging(20, 3); <span style="color: #3F7E5E">// 20 records per a page and current page number is 3</span>
     * Class&lt;SimpleMember&gt; entityType = SimpleMember.class;
     * ListResultBean&lt;SimpleMember&gt; memberList
     *     = memberBhv.outsideSql().freeStyle().pagingByCursorSkip().<span style="color: #DD4747">selectPagingListOnly</span>(path, pmb, entityType);
     * for (SimpleMember member : memberList) {
     *     ... = member.get...();
     * }
     * </pre>
     * The parameter-bean needs to extend SimplePagingBean.
     * The way to generate it is following:
     * <pre>
     * <span style="color: #3F7E5E">-- !df:pmb extends Paging!</span>
     * <span style="color: #3F7E5E">-- !!Integer memberId!!</span>
     * <span style="color: #3F7E5E">-- !!...!!</span>
     * </pre>
     * You don't need to use pagingBean's isPaging() method on your 'Parameter Comment'.
     * <pre>
     * e.g. ManualPaging and MySQL 
     * select member.MEMBER_ID
     *      , member.MEMBER_NAME
     *      , memberStatus.MEMBER_STATUS_NAME
     *   from MEMBER member
     *     left outer join MEMBER_STATUS memberStatus
     *       on member.MEMBER_STATUS_CODE = memberStatus.MEMBER_STATUS_CODE
     *  <span style="color: #3F7E5E">/*BEGIN*/</span>
     *  where
     *    <span style="color: #3F7E5E">/*IF pmb.memberId != null*/</span>
     *    member.MEMBER_ID = <span style="color: #3F7E5E">/*pmb.memberId*/</span>'123'
     *    <span style="color: #3F7E5E">/*END*/</span>
     *    <span style="color: #3F7E5E">/*IF pmb.memberName != null*/</span>
     *    and member.MEMBER_NAME like <span style="color: #3F7E5E">/*pmb.memberName*/</span>'Billy%'
     *    <span style="color: #3F7E5E">/*END*/</span>
     *  <span style="color: #3F7E5E">/*END*/</span>
     *  order by member.UPDATE_DATETIME desc
     *  limit <span style="color: #3F7E5E">/*$pmb.pageStartIndex*/</span>80, <span style="color: #3F7E5E">/*$pmb.fetchSize*/</span>20
     * </pre>
     * @param <ENTITY> The type of entity.
     * @param path The path of SQL that executes count and paging. (NotNull)
     * @param pmb The bean of paging parameter. (NotNull)
     * @param entityType The type of result entity. (NotNull)
     * @return The result bean of paged list. (NotNull)
     * @exception org.seasar.dbflute.exception.OutsideSqlNotFoundException When the outside-SQL is not found.
     * @exception org.seasar.dbflute.exception.DangerousResultSizeException When the result size is over the specified safety size.
     */
    def selectPagingListOnly[ENTITY](path: String, pmb: PagingBean, entityType: Class[ENTITY]): immutable.List[ENTITY] = {
        return toScalaList(executor.selectList(path, pmb, entityType));
    }

    // ===================================================================================
    //                                                                        Scala Helper
    //                                                                        ============
    protected def toScalaList[ENTITY](javaList: Collection[ENTITY]): immutable.List[ENTITY] = {
        if (javaList == null) { scala.collection.immutable.List() }
        return immutable.List.fromArray(javaList.toArray()).asInstanceOf[immutable.List[ENTITY]];
    }

    protected def createPagingView[ENTITY](selectedList: immutable.List[ENTITY], bean: PagingResultBean[_]): ScrPagingView[ENTITY] = {
        return new ScrPagingView[ENTITY](selectedList, bean);
    }
}
