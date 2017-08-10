import static org.junit.Assert.*;

import org.joda.time.IllegalFieldValueException;
import org.joda.time.Interval;
import org.joda.time.LocalDate;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.junit.Test;

public class Testes {

	@Test
	public void criaDataNatal2017() {
		// Fixture Setup
		// Exercise SUT
		LocalDate natal2017 = new LocalDate(2017, 12, 25);
		// Result Verification
		assertEquals(2017, natal2017.getYear());
		assertEquals(12, natal2017.getMonthOfYear());
		assertEquals(25, natal2017.getDayOfMonth());
		// Fixture Teardown
	}

	@Test
	public void CriaçãoDeData1() {
		// Fixture Setup
		// Exercise SUT
		LocalDate natal2017 = new LocalDate(2017, 12, 25);
		// Result Verification
		assertEquals(2017, natal2017.getYear());
		assertEquals(12, natal2017.getMonthOfYear());
		assertEquals(25, natal2017.getDayOfMonth());
		// Fixture Teardown
	}

	@Test
	public void CriaçãoDeData2() {
		// Fixture Setup
		// Exercise SUT
		LocalDate natal2017 = new LocalDate(2017, 1, 25);
		// Result Verification
		assertEquals(2017, natal2017.getYear());
		assertEquals(1, natal2017.getMonthOfYear());
		assertEquals(25, natal2017.getDayOfMonth());
		// Fixture Teardown
	}

	@Test
	public void CriaçãoDeData3() {
		// Fixture Setup
		// Exercise SUT
		LocalDate natal2017 = new LocalDate(-1, 1, 1);
		// Result Verification
		// Fixture Teardown
	}

	@Test
	public void CriaçãoDeData4() {
		// Fixture Setup
		// Exercise SUT
		LocalDate natal2017 = new LocalDate(-20, 10, 10);
		// Result Verification
		// Fixture Teardown
	}

	@Test
	public void criaDataDeAnoBissextoValido() {
		// Fixture Setup
		// Exercise SUT
		LocalDate dia29Fevereiro = new LocalDate(2016, 2, 29);
		// Result Verification
		// Fixture Teardown
	}

	@Test(expected = IllegalFieldValueException.class)
	public void CriaçãoDeDataFalha1() {
		// Fixture Setup
		// Exercise SUT
		LocalDate natal2017 = new LocalDate(2017, 12, -1);
		// Result Verification
		// Fixture Teardown
	}

	@Test(expected = IllegalFieldValueException.class)
	public void CriaçãoDeDataFalha2() {
		// Fixture Setup
		// Exercise SUT
		LocalDate natal2017 = new LocalDate(2017, -1, 1);
		// Result Verification
		// Fixture Teardown
	}

	@Test
	public void CriaçãoDeDataFalha3() {
		// Fixture Setup
		// Exercise SUT
		LocalDate natal2017 = new LocalDate(-1, 1, 1);
		// Result Verification
		// Fixture Teardown
	}

	@Test(expected = IllegalFieldValueException.class)
	public void CriaçãoDeDataFalha4() {
		// Fixture Setup
		// Exercise SUT
		LocalDate natal2017 = new LocalDate(-1, -1, -1);
		// Result Verification
		// Fixture Teardown
	}

	@Test(expected = IllegalFieldValueException.class)
	public void CriaçãoDeDataFalha5() {
		// Fixture Setup
		// Exercise SUT
		LocalDate natal2017 = new LocalDate(-1, -1, 1);
		// Result Verification
		// Fixture Teardown
	}

	@Test(expected = IllegalFieldValueException.class)
	public void CriaçãoDeDataFalha6() {
		// Fixture Setup
		// Exercise SUT
		LocalDate natal2017 = new LocalDate(0, -1, -1);
		// Result Verification
		// Fixture Teardown
	}

	@Test(expected = IllegalFieldValueException.class)
	public void CriaçãoDeDataFalha7() {
		// Fixture Setup
		// Exercise SUT
		LocalDate natal2017 = new LocalDate(-20, 1, 0);
		// Result Verification
		// Fixture Teardown
	}

	@Test(expected = IllegalFieldValueException.class)
	public void criaDatAFalhaDeAnoBissextoInvalido() {
		// Fixture Setup
		// Exercise SUT
		LocalDate dia29Fevereiro = new LocalDate(2017, 2, 29);
		// Result Verification
		// Fixture Teardown
	}

	@Test
	public void somaDeAnos() {
		// Fixture Setup
		LocalDate natal2017 = new LocalDate(2017, 12, 25);
		LocalDate natal2018 = new LocalDate(2018, 12, 25);

		// Exercise SUT
		LocalDate proximoNatal = natal2017.plusYears(1);

		// Result Verification
		assertEquals(natal2018, proximoNatal);

		// Fixture Teardown
	}

	@Test
	public void somaDeAnosFalha() {
		// Fixture Setup
		LocalDate natal2017 = new LocalDate(2017, 12, 25);
		LocalDate natal2018 = new LocalDate(2018, 12, 25);

		// Exercise SUT
		LocalDate proximoNatal = natal2017.plusYears(2);

		// Result Verification
		assertNotEquals(natal2018, proximoNatal);

		// Fixture Teardown
	}

	@Test
	public void subtraçãoDeDatas() {
		// Fixture Setup
		LocalDate natal2017 = new LocalDate(2017, 12, 25);
		LocalDate natal2016 = new LocalDate(2016, 12, 25);

		// Exercise SUT
		LocalDate ultimoNatal = natal2017.minusYears(1);

		// Result Verification
		assertEquals(natal2016, ultimoNatal);

		// Fixture Teardown
	}

	@Test
	public void subtraçãoDeDatasFalha() {
		// Fixture Setup
		LocalDate natal2017 = new LocalDate(2017, 12, 25);
		LocalDate natal2016 = new LocalDate(2018, 12, 25);

		// Exercise SUT
		LocalDate ultimoNatal = natal2017.minusYears(2);

		// Result Verification
		assertNotEquals(natal2016, ultimoNatal);

		// Fixture Teardown
	}

	@Test
	public void criaçãoDeHoras() {
		// Fixture Setup
		LocalTime horaAtual = new LocalTime(12, 12, 25);
		LocalTime horaEsperada = new LocalTime(11, 12, 25);

		// Exercise SUT

		// Result Verification
		assertTrue(horaAtual.getHourOfDay() > horaEsperada.getHourOfDay());

		// Fixture Teardown
	}

	@Test(expected = IllegalFieldValueException.class)
	public void criaçãoDeHorasFalha() {
		// Fixture Setup
		LocalTime horaAtual = new LocalTime(-12, 12, 25);
		// Exercise SUT
		// Result Verification
		// Fixture Teardown
	}

	@Test
	public void somaDeHoras() {
		// Fixture Setup
		LocalTime hora1 = new LocalTime(1, 12, 25);
		LocalTime hora2 = new LocalTime(2, 12, 25);
		LocalTime horaEsperada = new LocalTime(3, 12, 25);

		// Exercise SUT
		int novaHora = hora1.getHourOfDay() + hora2.getHourOfDay();

		// Result Verification
		assertEquals(novaHora, horaEsperada.getHourOfDay());

		// Fixture Teardown
	}

	@Test
	public void somaDeHorasFalha1() {
		// Fixture Setup
		LocalTime hora1 = new LocalTime(1, 12, 25);
		LocalTime hora2 = new LocalTime(2, 12, 25);
		LocalTime horaEsperada = new LocalTime(2, 12, 25);

		// Exercise SUT
		int novaHora = hora1.getHourOfDay() + hora2.getHourOfDay();

		// Result Verification
		assertNotEquals(novaHora, horaEsperada.getHourOfDay());

		// Fixture Teardown
	}

	@Test
	public void criaçãoDePeriodos() {
		// Fixture Setup
		Period hora1 = new Period(1);

		// Exercise SUT

		// Result Verification
		assertNotNull(hora1);

		// Fixture Teardown
	}

	@Test(expected = IllegalArgumentException.class)
	public void criaçãoDePeriodosFalha() {
		// Fixture Setup
		Period hora1 = new Period("1");

		// Exercise SUT

		// Result Verification

		// Fixture Teardown
	}

	@Test
	public void tamanhoDoPeriodoUmAno() {
		// Fixture Setup
		Period periodoUmAno = new Period(new LocalDate(2017, 12, 25),
				new LocalDate(2018, 12, 25));

		// Period(int years, int months, int weeks, int days, int hours, int
		// minutes, int seconds, int millis)
		Period periodoUmAnoEsperado = new Period(1, 0, 0, 0, 0, 0, 0, 0);

		// Exercise SUT

		// Result Verification
		assertEquals(periodoUmAno, periodoUmAnoEsperado);

		// Fixture Teardown
	}

	@Test
	public void tamanhoDoPeriodoUmAnoFalha() {
		// Fixture Setup
		Period periodoUmAno = new Period(new LocalDate(2017, 12, 25),
				new LocalDate(2019, 12, 25));

		// Period(int years, int months, int weeks, int days, int hours, int
		// minutes, int seconds, int millis)
		Period periodoUmAnoEsperado = new Period(1, 0, 0, 0, 0, 0, 0, 0);

		// Exercise SUT

		// Result Verification
		assertNotEquals(periodoUmAno, periodoUmAnoEsperado);

		// Fixture Teardown
	}

	@Test
	public void tamanhoDoPeriodoUmaSemana() {
		// Fixture Setup
		Period periodoUmAno = new Period(new LocalDate(2017, 12, 18),
				new LocalDate(2017, 12, 25));

		// Period(int years, int months, int weeks, int days, int hours, int
		// minutes, int seconds, int millis)
		Period periodoUmAnoEsperado = new Period(0, 0, 1, 0, 0, 0, 0, 0);

		// Exercise SUT

		// Result Verification
		assertEquals(periodoUmAno, periodoUmAnoEsperado);

		// Fixture Teardown
	}

	@Test
	public void tamanhoDoPeriodoUmaSemanaFalha() {
		// Fixture Setup
		Period periodoUmAno = new Period(new LocalDate(2017, 12, 17),
				new LocalDate(2017, 12, 25));

		// Period(int years, int months, int weeks, int days, int hours, int
		// minutes, int seconds, int millis)
		Period periodoUmAnoEsperado = new Period(0, 0, 1, 0, 0, 0, 0, 0);

		// Exercise SUT

		// Result Verification
		assertNotEquals(periodoUmAno, periodoUmAnoEsperado);

		// Fixture Teardown
	}
	
	@Test
	public void criaçãoDeIntervalos() {
		// Fixture Setup
		Interval hora1 = new Interval(0, 50);

		// Exercise SUT

		// Result Verification
		assertNotNull(hora1);

		// Fixture Teardown
	}

	@Test
	public void criaçãoDeIntervalosVazios() {
		// Fixture Setup
		Interval hora1 = new Interval(0, 0);

		// Exercise SUT

		// Result Verification
		assertNotNull(hora1);

		// Fixture Teardown
	}

	@Test
	public void criaçãoDeIntervalosVaziosNegativos() {
		// Fixture Setup
		Interval hora1 = new Interval(-1, -1);

		// Exercise SUT

		// Result Verification
		assertNotNull(hora1);

		// Fixture Teardown
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void criaçãoDeIntervalosFalha() {
		// Fixture Setup
		Interval hora1 = new Interval(0, -50);

		// Exercise SUT

		// Result Verification
		assertNotNull(hora1);

		// Fixture Teardown
	}
	// http://www.joda.org/joda-time/apidocs/index.html
	// http://www.joda.org/joda-time/userguide.html#Intervals
	@Test
	public void criaçãoDataHora() {
		// Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
		DateTime dataHora = new DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour);

		// Exercise SUT

		// Result Verification
		assertNotNull(hora1);

		// Fixture Teardown
	}
}
