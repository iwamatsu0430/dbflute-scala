package com.example.dbflute.scala.dbflute.allcommon;

import scala.collection.JavaConverters._

import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

import org.seasar.dbflute.Entity;
import org.seasar.dbflute.dbmeta.DBMeta;
import org.seasar.dbflute.dbmeta.DBMetaProvider;
import org.seasar.dbflute.exception.DBMetaNotFoundException;
import org.seasar.dbflute.helper.StringKeyMap;
import org.seasar.dbflute.util.DfAssertUtil;

/**
 * The handler of the instance of DB meta.
 * @author DBFlute(AutoGenerator)
 */
object DBMetaInstanceHandler extends DBMetaProvider {

    // ===================================================================================
    //                                                                        Resource Map
    //                                                                        ============
    /** The map of DB meta instance by key: table DB-name. */
    protected val _tableDbNameInstanceMap: Map[String, DBMeta] = new HashMap();

    /** The map of DB meta instance by key: entity type. */
    protected val _entityTypeInstanceMap: Map[Class[_], DBMeta] = new HashMap();

    /** The map of table DB name and DB meta class name. */
    protected var _tableDbNameClassNameMap: Map[String, String] = null;
    val tmpMap: Map[String, String] = new HashMap();
    tmpMap.put("MEMBER", "com.example.dbflute.scala.dbflute.bsentity.dbmeta.MemberDbm");
    _tableDbNameClassNameMap = Collections.unmodifiableMap(tmpMap);

    /** The flexible map of table DB name. This is for conversion at finding. */
    protected val _tableDbNameFlexibleMap: Map[String, String] = StringKeyMap.createAsFlexible();
    val tableDbNameSet: Set[String] = _tableDbNameClassNameMap.keySet();
    tableDbNameSet.asScala.foreach(tableDbName => _tableDbNameFlexibleMap.put(tableDbName, tableDbName))

    /**
     * Get the unmodifiable map of DB meta.
     * @return The unmodifiable map that contains all instances of DB meta. (NotNull & NotEmpty)
     */
    def getUnmodifiableDBMetaMap(): Map[String, DBMeta] = {
        initializeDBMetaMap();
        _tableDbNameInstanceMap.synchronized {
            return Collections.unmodifiableMap(_tableDbNameInstanceMap);
        }
    }

    /**
     * Initialize the map of DB meta.
     */
    protected def initializeDBMetaMap(): Unit = {
        if (isInitialized()) {
            return;
        }
        _tableDbNameInstanceMap.synchronized {
            _tableDbNameClassNameMap.keySet().asScala.foreach(tableDbName => findDBMeta(tableDbName)); // initialize
            if (!isInitialized()) {
                val msg: String = "Failed to initialize tableDbNameInstanceMap: " + _tableDbNameInstanceMap;
                throw new IllegalStateException(msg);
            }
        }
    }

    protected def isInitialized(): Boolean = {
        return _tableDbNameInstanceMap.size() == _tableDbNameClassNameMap.size();
    }

    // ===================================================================================
    //                                                                  Provider Singleton
    //                                                                  ==================
    protected val _provider: DBMetaProvider = this;

    def getProvider(): DBMetaProvider = {
        return _provider;
    }

    def provideDBMeta(tableFlexibleName: String): DBMeta = {
        return byTableFlexibleName(tableFlexibleName);
    }

    def provideDBMeta(entityType: Class[_]): DBMeta = {
        return byEntityType(entityType);
    }

    def provideDBMetaChecked(tableFlexibleName: String): DBMeta = {
        return findDBMeta(tableFlexibleName);
    }

    def provideDBMetaChecked(entityType: Class[_]): DBMeta = {
        return findDBMeta(entityType);
    }

    // ===================================================================================
    //                                                                         Find DBMeta
    //                                                                         ===========
    /**
     * Find DB meta by table flexible name. (accept quoted name and schema prefix)
     * @param tableFlexibleName The flexible name of table. (NotNull)
     * @return The instance of DB meta. (NotNull)
     * @exception org.seasar.dbflute.exception.DBMetaNotFoundException When the DB meta is not found.
     */
    def findDBMeta(tableFlexibleName: String): DBMeta = {
        val dbmeta: DBMeta = byTableFlexibleName(tableFlexibleName);
        if (dbmeta == null) {
            val msg: String = "The DB meta was not found by the table flexible name: key=" + tableFlexibleName;
            throw new DBMetaNotFoundException(msg);
        }
        return dbmeta;
    }

    /**
     * Find DB meta by entity type.
     * @param entityType The entity type of table, which should implement the {@link Entity} interface. (NotNull)
     * @return The instance of DB meta. (NotNull)
     * @exception org.seasar.dbflute.exception.DBMetaNotFoundException When the DB meta is not found.
     */
    def findDBMeta(entityType: Class[_]): DBMeta = {
        val dbmeta: DBMeta = byEntityType(entityType);
        if (dbmeta == null) {
            val msg: String = "The DB meta was not found by the entity type: key=" + entityType;
            throw new DBMetaNotFoundException(msg);
        }
        return dbmeta;
    }

    // ===================================================================================
    //                                                                       by Table Name
    //                                                                       =============
    /**
     * @param tableFlexibleName The flexible name of table. (NotNull)
     * @return The instance of DB meta. (NullAllowed: If the DB meta is not found, it returns null)
     */
    protected def byTableFlexibleName(tableFlexibleName: String): DBMeta = {
        assertStringNotNullAndNotTrimmedEmpty("tableFlexibleName", tableFlexibleName);
        var tableDbName: String = _tableDbNameFlexibleMap.get(tableFlexibleName);
        if (tableDbName == null) {
            tableDbName = retryByNormalizedName(tableFlexibleName);
        }
        return if (tableDbName != null) { byTableDbName(tableDbName) } else { null };
    }

    protected def retryByNormalizedName(tableFlexibleName: String): String = {
        var tableDbName: String = null;
        val pureName: String = normalizeTableFlexibleName(tableFlexibleName);
        val schema: String = extractSchemaIfExists(tableFlexibleName);
        if (schema != null) { // first, find by qualified name
            tableDbName = _tableDbNameFlexibleMap.get(schema + "." + pureName);
        }
        if (tableDbName == null) { // next, find by pure name
            tableDbName = _tableDbNameFlexibleMap.get(pureName);
        }
        return tableDbName;
    }

    protected def normalizeTableFlexibleName(tableFlexibleName: String): String = {
        return removeQuoteIfExists(removeSchemaIfExists(tableFlexibleName));
    }

    protected def removeQuoteIfExists(name: String): String = {
        if (name.startsWith("\"") && name.endsWith("\"")) {
            return strip(name);
        } else if (name.startsWith("[") && name.endsWith("]")) {
            return strip(name);
        }
        return name;
    }

    protected def removeSchemaIfExists(name: String): String = {
        val dotLastIndex: Integer = name.lastIndexOf(".");
        return if (dotLastIndex >= 0) { name.substring(dotLastIndex + ".".length()) } else { name }; 
    }

    protected def extractSchemaIfExists(name: String): String = {
        val dotLastIndex: Integer = name.lastIndexOf(".");
        return if (dotLastIndex >= 0) { name.substring(0, dotLastIndex) } else { null }; 
    }

    protected def strip(name: String): String = {
        return name.substring(1, name.length() - 1);
    }

    /**
     * @param tableDbName The DB name of table. (NotNull)
     * @return The instance of DB meta. (NullAllowed: If the DB meta is not found, it returns null)
     */
    protected def byTableDbName(tableDbName: String): DBMeta = {
        assertStringNotNullAndNotTrimmedEmpty("tableDbName", tableDbName);
        return getCachedDBMeta(tableDbName);
    }

    // ===================================================================================
    //                                                                      by Entity Type
    //                                                                      ==============
    /**
     * @param entityType The entity type of table, which should implement the entity interface. (NotNull)
     * @return The instance of DB meta. (NullAllowed: If the DB meta is not found, it returns null)
     */
    protected def byEntityType(entityType: Class[_]): DBMeta = {
        assertObjectNotNull("entityType", entityType);
        return getCachedDBMeta(entityType);
    }

    // ===================================================================================
    //                                                                       Cached DBMeta
    //                                                                       =============
    protected def getCachedDBMeta(tableDbName: String): DBMeta = { // lazy-load (thank you koyak!)
        var dbmeta: DBMeta = _tableDbNameInstanceMap.get(tableDbName);
        if (dbmeta != null) {
            return dbmeta;
        }
        _tableDbNameInstanceMap.synchronized {
            dbmeta = _tableDbNameInstanceMap.get(tableDbName);
            if (dbmeta != null) {
                // an other thread might have initialized
                // or reading might failed by same-time writing
                return dbmeta;
            }
            val dbmetaName: String = _tableDbNameClassNameMap.get(tableDbName);
            if (dbmetaName == null) {
                return null;
            }
            _tableDbNameInstanceMap.put(tableDbName, toDBMetaInstance(dbmetaName));
            return _tableDbNameInstanceMap.get(tableDbName);
        }
    }

    protected def toDBMetaInstance(dbmetaName: String): DBMeta = {
        try {
            val dbmetaType: Class[_] = Class.forName(dbmetaName);
            val field: Field = dbmetaType.getField("MODULE$");
            val result: Object = field.get(null);
            return result.asInstanceOf[DBMeta];
        } catch {
            case e: Exception => {
                val msg: String = "Failed to get the instance: " + dbmetaName;
                throw new IllegalStateException(msg, e);
            }
        }
    }

    protected def getCachedDBMeta(entityType: Class[_]): DBMeta = { // lazy-load same as by-name
        var dbmeta: DBMeta = _entityTypeInstanceMap.get(entityType);
        if (dbmeta != null) {
            return dbmeta;
        }
        _entityTypeInstanceMap.synchronized {
            dbmeta = _entityTypeInstanceMap.get(entityType);
            if (dbmeta != null) {
                // an other thread might have initialized
                // or reading might failed by same-time writing
                return dbmeta;
            }
            if (classOf[Entity].isAssignableFrom(entityType)) { // required
                val entity: Entity = newEntity(entityType);
                dbmeta = getCachedDBMeta(entity.getTableDbName());
            }
            if (dbmeta == null) {
                return null;
            }
            _entityTypeInstanceMap.put(entityType, dbmeta);
            return _entityTypeInstanceMap.get(entityType);
        }
    }

    protected def newEntity(entityType: Class[_]): Entity = {
        try {
            return entityType.newInstance().asInstanceOf[Entity];
        } catch {
            case e: Exception => {
                val msg: String = "Failed to new the instance: " + entityType;
                throw new IllegalStateException(msg, e);
            }
        }
    }

    // ===================================================================================
    //                                                                      General Helper
    //                                                                      ==============
    // -----------------------------------------------------
    //                                         Assert Object
    //                                         -------------
    protected def assertObjectNotNull(variableName: String, value: Object): Unit = {
        DfAssertUtil.assertObjectNotNull(variableName, value);
    }

    // -----------------------------------------------------
    //                                         Assert String
    //                                         -------------
    protected def assertStringNotNullAndNotTrimmedEmpty(variableName: String, value: String): Unit = {
        DfAssertUtil.assertStringNotNullAndNotTrimmedEmpty(variableName, value);
    }
}
