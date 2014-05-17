package com.example.dbflute.scala.dbflute.cbean.cq.ciq;

import java.util.Map;
import org.seasar.dbflute.cbean._
import org.seasar.dbflute.cbean.ckey._
import org.seasar.dbflute.cbean.coption.ConditionOption;
import org.seasar.dbflute.cbean.cvalue.ConditionValue;
import org.seasar.dbflute.cbean.sqlclause.SqlClause;
import org.seasar.dbflute.exception.IllegalConditionBeanOperationException;
import com.example.dbflute.scala.dbflute.cbean._
import com.example.dbflute.scala.dbflute.cbean.cq.bs._
import com.example.dbflute.scala.dbflute.cbean.cq._

/**
 * The condition-query for in-line of MEMBER_SERVICE.
 * @author DBFlute(AutoGenerator)
 */
class MemberServiceCIQ(childQuery: ConditionQuery, sqlClause: SqlClause, aliasName: String, nestLevel: Integer, myCQ: BsMemberServiceCQ)
        extends AbstractBsMemberServiceCQ(childQuery, sqlClause, aliasName, nestLevel) {

    // ===================================================================================
    //                                                                           Attribute
    //                                                                           =========
    protected var _myCQ: BsMemberServiceCQ = null;

    // ===================================================================================
    //                                                                         Constructor
    //                                                                         ===========
    {
        _myCQ = myCQ;
        _foreignPropertyName = _myCQ.xgetForeignPropertyName(); // accept foreign property name
        _relationPath = _myCQ.xgetRelationPath(); // accept relation path
        _inline = true;
    }

    // ===================================================================================
    //                                                             Override about Register
    //                                                             =======================
    override protected def reflectRelationOnUnionQuery(bq: ConditionQuery, uq: ConditionQuery): Unit = {
        val msg: String = "InlineView must not need UNION method: " + bq + " : " + uq;
        throw new IllegalConditionBeanOperationException(msg);
    }

    override protected def setupConditionValueAndRegisterWhereClause(k: ConditionKey, v: Object, cv: ConditionValue, col: String): Unit = {
        regIQ(k, v, cv, col);
    }

    override protected def setupConditionValueAndRegisterWhereClause(k: ConditionKey, v: Object, cv: ConditionValue, col: String, op: ConditionOption): Unit = {
        regIQ(k, v, cv, col, op);
    }

    override protected def registerWhereClause(wc: String): Unit = {
        registerInlineWhereClause(wc);
    }

    override protected def isInScopeRelationSuppressLocalAliasName(): Boolean = {
        if (_onClause) {
            throw new IllegalConditionBeanOperationException("InScopeRelation on OnClause is unsupported.");
        }
        return true;
    }

    // ===================================================================================
    //                                                                Override about Query
    //                                                                ====================
    protected def getCValueMemberServiceId(): ConditionValue = { return _myCQ.getMemberServiceId(); }
    protected def getCValueMemberId(): ConditionValue = { return _myCQ.getMemberId(); }
    protected def getCValueServicePointCount(): ConditionValue = { return _myCQ.getServicePointCount(); }
    protected def getCValueServiceRankCode(): ConditionValue = { return _myCQ.getServiceRankCode(); }
    protected def getCValueRegisterDatetime(): ConditionValue = { return _myCQ.getRegisterDatetime(); }
    protected def getCValueRegisterUser(): ConditionValue = { return _myCQ.getRegisterUser(); }
    protected def getCValueUpdateDatetime(): ConditionValue = { return _myCQ.getUpdateDatetime(); }
    protected def getCValueUpdateUser(): ConditionValue = { return _myCQ.getUpdateUser(); }
    protected def getCValueVersionNo(): ConditionValue = { return _myCQ.getVersionNo(); }
    protected def xfindFixedConditionDynamicParameterMap(pp: String): Map[String, Object] = { return null; }
    def keepScalarCondition(sq: MemberServiceCQ): String =
    { throwIICBOE("ScalarCondition"); return null; }
    def keepSpecifyMyselfDerived(sq: MemberServiceCQ): String =
    { throwIICBOE("(Specify)MyselfDerived"); return null;}
    def keepQueryMyselfDerived(sq: MemberServiceCQ): String =
    { throwIICBOE("(Query)MyselfDerived"); return null;}
    def keepQueryMyselfDerivedParameter(vl: Object): String =
    { throwIICBOE("(Query)MyselfDerived"); return null;}
    def keepMyselfExists(sq: MemberServiceCQ): String =
    { throwIICBOE("MyselfExists"); return null;}
    def keepMyselfInScope(sq: MemberServiceCQ): String =
    { throwIICBOE("MyselfInScope"); return null;}

    protected def throwIICBOE(name: String): Unit = { // throwInlineIllegalConditionBeanOperationException()
        throw new IllegalConditionBeanOperationException(name + " at InlineView is unsupported.");
    }

    // ===================================================================================
    //                                                                       Very Internal
    //                                                                       =============
    // very internal (for suppressing warn about 'Not Use Import')
    protected def xinCB(): String = { return classOf[MemberServiceCB].getName(); }
    protected def xinCQ(): String = { return classOf[MemberServiceCQ].getName(); }
}
