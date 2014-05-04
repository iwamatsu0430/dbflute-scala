package org.dbflute.scala.testlib.dbflute.nogen;

import org.dbflute.scala.testlib.dbflute.allcommon.DBFluteConfig;
import org.dbflute.scala.testlib.dbflute.allcommon.DBFluteInitializer;
import org.seasar.dbflute.DBDef;
import org.seasar.dbflute.bhv.core.BehaviorCommandInvoker;
import org.seasar.dbflute.jdbc.StatementConfig;
import org.seasar.dbflute.outsidesql.OutsideSqlOption;
import org.seasar.dbflute.outsidesql.executor.OutsideSqlBasicExecutor;
import org.seasar.dbflute.outsidesql.factory.DefaultOutsideSqlExecutorFactory;


/**
 * @author jflute
 */
public class ExtendedDBFluteInitializer extends DBFluteInitializer {

    public ExtendedDBFluteInitializer() {
    }

    @Override
    protected void prologue() { // the example for fixed DBFluteConfig settings
        super.prologue(); // you need to call this

        final DBFluteConfig config = DBFluteConfig.getInstance();
        config.unlock();

        config.setOutsideSqlExecutorFactory(new DefaultOutsideSqlExecutorFactory() {
            @Override
            public <BEHAVIOR> OutsideSqlBasicExecutor<BEHAVIOR> createBasic(
                    BehaviorCommandInvoker behaviorCommandInvoker, String tableDbName, DBDef currentDBDef,
                    StatementConfig defaultStatementConfig, OutsideSqlOption outsideSqlOption) {
                final OutsideSqlBasicExecutor<BEHAVIOR> executor = super.createBasic(behaviorCommandInvoker,
                        tableDbName, currentDBDef, defaultStatementConfig, outsideSqlOption);
                return executor.removeLineComment().removeBlockComment(); // the test of removing
            }
        });

        config.lock();
    }
}
